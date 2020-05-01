module Wizard
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open MonoGame.Extended
open Sprites
open RigidBody
open System
open Microsoft.Xna.Framework.Input
open AnimationBank
open HurtIntent
open Element

type Health =
  { maxHealth: int
    health: int }
  member this.add amount = { this with health = min this.maxHealth (max 0 (this.health + amount)) }
  member this.IsFull = this.maxHealth = this.health
  member this.IsDead = this.health = 0
  member this.IsAlive = this.health > 0
  static member draw (health: Health) (characterBottom: Vector2) (characterHalfWidth: float32) (sb: SpriteBatch) =
    let maxHealthRectangle =
        Rectangle
          ( (characterBottom + Vector2(-characterHalfWidth - 1.f, 3.f)).ToPoint()
          , Point(int (characterHalfWidth * 2.f + 2.f), 7)
          )
    sb.DrawRectangle(maxHealthRectangle.ToRectangleF(), Color.Black, 7.f)
    let healthRectangle = Rectangle(maxHealthRectangle.X + 1, maxHealthRectangle.Y + 1, int (characterHalfWidth * 2.f * float32 health.health / float32 health.maxHealth), maxHealthRectangle.Size.Y - 2)
    sb.DrawRectangle(healthRectangle.ToRectangleF(), Color.Red, if health.health = 0 then 0.f else 5.f)

module Health =
    let create full = { maxHealth = full; health = full }
    let add amount (health: Health) = health.add amount
(*
type WizardStatus =
    //| SelfCasting of startTime: GameTime
    //| BeamCasting of startTime: GameTime
    //| Slashing of startTime: GameTime
    | Walking
    | Idle

type EffectType =
    | Heal of amount: int
    | PhysicalDamage of amount: int

type Effect =
    { sender: string
      effectType: EffectType }

type WizardAction =
    | WalkTo of Vector2
    | Conjure of Element 
    //| DoSlash of facing: Vector2
*)

type WizardUpdate =
    { movementVelocity: Vector2
      animationId: AnimationId
      heal: int
      walking: bool
      elements: ElementCollection
      channelStartTotalSeconds: float32 option
      spawnBoulder: {| direction: Vector2; castCompleteness: float32; earthCount: int; hurtMediums: HurtMediumMap |} option
    }

type Wizard =
  { name: string
    color: Color
    bankAnimation: BankAnimation
    bodyLayers: TextureKind array
    speed: float32
    walking: bool
    elements: ElementCollection
    rigidBody: RigidBody
    health: Health
    channelStartTotalSeconds: float32 option }
    member this.CurrentAnimation = this.bankAnimation.animation
    member this.Size with get() = this.CurrentAnimation.size

type CastingType =
    | SelfCast
    | TargetCastStart
    | TargetCastEnd
    //| AreaCast

type WizardInput =
    { cast: CastingType option
      walk: bool
      slash: bool
      newElement: Element option }
    static member empty = { cast = None; walk = false; slash = false; newElement = None }

module Wizard =
    open MonoGameExtensions.InputState
    open AnimationBank

    let getCasting (inputState: InputState) =
        match inputState with
        | MouseDown MiddleMouseButton -> Some SelfCast
        | MouseClick RightMouseButton & KeyUp Keys.LeftShift -> Some TargetCastStart
        | MouseRelease RightMouseButton -> Some TargetCastEnd
        | _ -> None

    let getInput (wizard: Wizard) (inputState: InputState) =
        let casting = getCasting inputState
        let slash =
            match inputState with
            | KeyDown Keys.LeftShift & MouseClick LeftMouseButton when not wizard.walking -> true
            | _ -> false
        let walk =
            match inputState with
            | MouseDown LeftMouseButton -> true
            | _ -> false
        { cast = casting
          walk = walk
          slash = slash
          newElement = Element.fromKeyboard inputState.prevKeyb inputState.currKeyb }

    let defaultHurtMediumMap (elements: ElementCollection) =
        elements.Elements
        |> List.map (function
            | Life -> Elemental Life, 178
            | Earth -> Physical, 60
        )
        |> HurtMediumMap.ofSeq

    let hurtIntent (gameTime: GameTime) (wizard: Wizard) =
        let noAttack = HurtIntent.create wizard.name wizard.rigidBody.aabb
        match wizard.bankAnimation.animationId.animationKind with
        | Slash ->
            let animation = wizard.bankAnimation.animation
            if animation.currentFrame + 3 = animation.frames.Length && animation.frameTimer + gameTime.ElapsedGameTime > animation.frameLength then
                let attackAABB = AABB.create (wizard.rigidBody.aabb.center, (wizard.rigidBody.aabb.halfExtents + Vector2(13.f, 13.f)))
                HurtIntent.createSimple wizard.name wizard.rigidBody.aabb attackAABB [Physical, 130]
            else
                noAttack
        | _ -> noAttack

    let updateWizard (friction: float32) (gameTime: GameTime) (walkingTarget: Vector2) (input: WizardInput) (recieveHurt: HurtIntent list) (wizard: Wizard) =
        let maxChannelSeconds = 4.f
        let attackIntents (recieveHurt: HurtIntent list) =
            recieveHurt
            |> Seq.collect (fun hi ->
                hi.attackIntent
                |> Option.toList
                |> Seq.map (fun ai -> ai.mediums)
            )
            |> HurtMediumMap.unionMany
            |> HurtMediumMap.toSeq
        let calculateHpDiff (recieveHurt: seq<HurtMedium * int>) =
            recieveHurt
            |> Seq.fold (fun heal (medium,amount) ->
                match medium with
                | Physical -> heal - amount
                | Elemental Life -> heal + amount
                | _ -> heal
            ) 0
        let heal = recieveHurt |> attackIntents |> calculateHpDiff
        let movementVector = (walkingTarget - wizard.rigidBody.aabb.center) |> Vector2.NormalizeOrZero
        let walking =
            input.walk &&
            Vector2.Clamp(walkingTarget, wizard.rigidBody.aabb.min, wizard.rigidBody.aabb.max) <> walkingTarget
        let facing =
            AnimationFacing.ofVector movementVector
        let movementVelocity =
            if walking then movementVector * wizard.speed * friction else Vector2.Zero
        let defaultUpdate =
            { movementVelocity = movementVelocity
              animationId = wizard.bankAnimation.animationId
              walking = walking
              heal = heal
              elements = wizard.elements
              channelStartTotalSeconds = wizard.channelStartTotalSeconds
              spawnBoulder = None }
        let animation = AnimationId.create facing
        if wizard.health.add(heal).health = 0 then
            { defaultUpdate with
                movementVelocity = Vector2.Zero
                animationId = animation Hurt
                walking = false
                elements = ElementCollection.empty }
        else
        match wizard.channelStartTotalSeconds with
        | None ->
            match wizard.bankAnimation.animationId.animationKind with
            | Idle | Walkcycle ->
                let spell = wizard.elements |> ElementCollection.spell
                match input.cast, spell with
                | Some SelfCast, Some LifeSpell ->
                    let heal' =
                        wizard.elements.Elements
                        |> Seq.where ((=)Life)
                        |> Seq.map (fun _ -> Elemental Life, 178)
                        |> calculateHpDiff
                    { defaultUpdate with
                        movementVelocity = Vector2.Zero
                        animationId = animation Spellcast
                        walking = false
                        heal = heal + heal'
                        elements = ElementCollection.empty }
                | Some TargetCastStart, Some (ProjectileSpell EarthProjectile) ->
                    { defaultUpdate with
                        movementVelocity = Vector2.Zero
                        animationId = animation Spellcast
                        walking = false
                        channelStartTotalSeconds = Some (float32 gameTime.TotalGameTime.TotalSeconds)}
                | None, _ when input.slash ->
                    { defaultUpdate with
                        movementVelocity = Vector2.Zero
                        animationId = AnimationId.createWeapon facing Slash SlashDagger
                        walking = false }
                | _ ->
                    let newElements =
                        match input.newElement with
                        | None -> wizard.elements
                        | Some e -> ElementCollection.conjure e wizard.elements
                    { defaultUpdate with
                        movementVelocity = movementVelocity * (1.f - (float32 newElements.Length * 0.1f))
                        animationId = animation (if walking then Walkcycle else Idle)
                        elements = newElements }
            | Spellcast | Slash ->
                { defaultUpdate with
                    movementVelocity = Vector2.Zero
                    animationId = if wizard.bankAnimation.animation.IsOver then animation Idle else wizard.bankAnimation.animationId
                    walking = false }
            | Hurt ->
                { defaultUpdate with
                    movementVelocity = Vector2.Zero
                    walking = false
                    heal = 0 }
        | Some channelStart ->
            match input.cast with
            | Some TargetCastEnd ->
                let castLength = min 1.f ((float32 gameTime.TotalGameTime.TotalSeconds - channelStart) / maxChannelSeconds)
                { defaultUpdate with
                    animationId = animation (if walking then Walkcycle else Idle)
                    spawnBoulder =
                        Some {| direction = movementVector
                                castCompleteness = castLength
                                earthCount = wizard.elements.Elements |> List.fold (fun n -> function | Earth -> n+1 | _ -> n ) 0
                                hurtMediums = defaultHurtMediumMap wizard.elements |}
                    channelStartTotalSeconds = None
                    elements = ElementCollection.empty }
            | None ->
                { defaultUpdate with
                    movementVelocity = Vector2.Zero
                    walking = false }

    let updateAnimation (ab: AnimationBank) (animationId: AnimationId) gameTime wizard =
        let animation =
            if wizard.bankAnimation.animationId = animationId then
                wizard.bankAnimation
            else
                ab.InitAnimation animationId wizard.bodyLayers
        animation |> BankAnimation.updateAnimation gameTime

    let draw (wizard: Wizard) (gameTime: GameTime) (sb: SpriteBatch) =
        let visualOffset = (wizard.rigidBody.aabb.halfExtents * Vector2(1.f,2.f)- wizard.CurrentAnimation.size.ToVector2() * Vector2(0.5f, 1.f))//  - Vector2(17.f,0.f)
        for (layerKind, texture) in wizard.bankAnimation.textures do
            let color =
                match layerKind with
                | Body Male -> Color.Black
                | Weapon _ -> Color.White
                | _ -> wizard.color
            sb.Draw(texture, wizard.rigidBody.aabb.min + visualOffset, Nullable.op_Implicit wizard.CurrentAnimation.CurrentFrame, color)
        sb.DrawRectangle(Rectangle(wizard.rigidBody.aabb.min.ToPoint(), wizard.rigidBody.aabb.size.ToPoint()).ToRectangleF(), wizard.color)
        Health.draw wizard.health (wizard.rigidBody.aabb.center + Vector2(0.f, wizard.rigidBody.aabb.halfExtents.Y)) wizard.rigidBody.aabb.halfExtents.X sb