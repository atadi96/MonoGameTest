module AnimationBank
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Sprites
open Microsoft.Xna.Framework

type Belt =
    | Leather
    | Rope

type Body =
    | Male
    | Skeleton

type Feet =
    | PlateArmorShoes
    | ShoesBrown

type Hands =
    | PlateArmorGloves

type Head =
    | ChainArmorHelmet
    | ChainArmorHood
    | HairBlonde
    | LeatherArmorHat
    | PlateArmorHelmet
    | RobeHood

type Legs =
    | PantsGreenish
    | PlareArmorPants
    | RobeSkirt

type Torso =
    | ChainArmorJacketPurple
    | ChainArmorTorso
    | LeatherArmorBracers
    | LeatherArmorShirtWhite
    | LeatherArmorShoulders
    | LeatherArmorTorso
    | PlateArmorArmsShoulders
    | PlateArmorTorso
    | RobeShirtBrown

type Weapon =
    | SlashDagger
    | ThrustSpear
    | ThrustStaff
    | BowBow
    static member All =
      [ SlashDagger
        ThrustSpear
        ThrustStaff
        BowBow ]

type TextureKind =
    | Body of Body
    | Feet of Feet
    | Hands of Hands
    | Head of Head
    | Legs of Legs
    | Torso of Torso
    | Belt of Belt
    | Weapon of Weapon

type AnimationKind =
    | Hurt
    | Bow
    | Slash
    | Spellcast
    | Thrust
    | Walkcycle
    | Idle
    static member AllTextureNames =
        [ Hurt
          Bow
          Slash
          Spellcast
          Thrust
          Walkcycle
          Idle ]
(*
module AnimationKind =
    let sourceTexture (aK: AnimationKind) =
        match aK with
        | BowWeapon -> Bow
        | SlashWeapon -> Slash
        | ThrustWeapon -> Thrust
        | Idle -> Walkcycle
        | ak -> ak
*)
type TextureId =
    { animationKind: AnimationKind
      textureKind: TextureKind }
    static member Create(animationKind, textureKind) = { animationKind = animationKind; textureKind = textureKind }

type AnimationFacing =
    | UpFacing
    | RightFacing
    | DownFacing
    | LeftFacing

module AnimationFacing =
    let ofVector (direction: Vector2) =
        if direction = Vector2.Zero then
            DownFacing
        else
        match atan2 direction.Y direction.X with
        | x when x < - 3.f * MathHelper.PiOver4 -> LeftFacing
        | x when x < -MathHelper.PiOver4 -> UpFacing
        | x when x < MathHelper.PiOver4 -> RightFacing
        | x when x < 3.f * MathHelper.PiOver4 -> DownFacing
        | _ -> LeftFacing

type AnimationId =
    { facing: AnimationFacing
      animationKind: AnimationKind
      weapon: Weapon option }
    static member create facing kind = { facing = facing; animationKind = kind; weapon = None }
    static member createWeapon facing kind weapon = { facing = facing; animationKind = kind; weapon = Some weapon }

let getAnimationResourceName (textureId: TextureId) =
    let animationKind =
        match textureId.animationKind with
        | Idle -> Walkcycle
        | _ -> textureId.animationKind
    let folder = animationKind.ToString().ToLower()
    let [| textureKind; textureName |] = textureId.textureKind.ToString().Split(' ')
    let rec nameParts (textureName: System.Char list) =
        match textureName with
        | c::cs when System.Char.IsUpper c ->
            '_' :: System.Char.ToLower c :: nameParts cs
        | c::cs -> c :: nameParts cs
        | [] -> []
    let resource =
        textureName.ToCharArray()
        |> Array.toList
        |> nameParts
        |> Seq.fold (fun (sb: System.Text.StringBuilder) c -> sb.Append(c)) (System.Text.StringBuilder(textureKind.ToUpper()))
    System.IO.Path.Combine[|folder;resource.ToString()|]

let getWeaponResourceName (weapon: Weapon) =
    let rec kindName (kind: string) (rest: char list) =
        match kind, rest with
        | "", c::cs ->
            kindName (System.Char.ToLower c |> string) cs
        | _, c::_ when System.Char.IsUpper c ->
            kind, (rest |> Array.ofList |> System.String).ToLower()
        | _, c::cs -> kindName (kind + string c) cs
        | _ -> failwith "inveriants rekt"
    kindName "" (weapon.ToString().ToCharArray() |> List.ofArray)
    |> fun (a,b) -> System.IO.Path.Combine [| a; "WEAPON_" + b |]

type BankAnimation =
  { animation: Animation
    animationId: AnimationId
    textures: (TextureKind * Texture2D) array }

module BankAnimation =
    let updateAnimation (gameTime: GameTime) (ba: BankAnimation) = { ba with animation = Sprites.Animation.update gameTime ba.animation}


type AnimationBank(prefix: string, content: ContentManager) =
    let mutable loadedTextures = Map.empty<TextureId, Texture2D>
    let mutable loadedWeapons = Map.empty<Weapon, Texture2D>

    member __.LoadTexture (textureId: TextureId) =
        match loadedTextures |> Map.tryFind textureId with
        | Some texture -> texture
        | None ->
            let resourceName = System.IO.Path.Combine [| prefix; getAnimationResourceName textureId |]
            let texture = content.Load<Texture2D>(resourceName)
            loadedTextures <- loadedTextures |> Map.add textureId texture
            loadedTextures <-
                match textureId.animationKind with
                | Idle -> loadedTextures |> Map.add { textureId with animationKind = Walkcycle } texture
                | Walkcycle -> loadedTextures |> Map.add { textureId with animationKind = Idle } texture
                | _ -> loadedTextures
            texture

    member __.LoadWeapons () =
        loadedWeapons <-
            Weapon.All
            |> Seq.map (fun w ->
                let resourceName = System.IO.Path.Combine [| prefix; getWeaponResourceName w |]
                w, content.Load<Texture2D>(resourceName)
            )
            |> Map.ofSeq

    static member Animation (animationId: AnimationId) =
        let facingId =
            match animationId.facing with
            | UpFacing -> 0
            | LeftFacing -> 1
            | DownFacing -> 2
            | RightFacing -> 3
        let facingOffset = Point(64,64*facingId)
        let size = Point(64,64)
        match animationId.animationKind with
        | Hurt -> Animation.Create(5, 5, size, Point(64,0), false)
        | Bow -> Animation.Create(12, 12, size, facingOffset, false)
        | Slash -> Animation.Create(5, 13, size, facingOffset, false)
        | Spellcast -> Animation.Create(6, 13, size, facingOffset, false)
        | Thrust -> Animation.Create(7, 7, size, facingOffset, false)
        | Walkcycle -> Animation.Create(8, 24, size, facingOffset, true)
        | Idle -> Animation.Create(1, 1, size, Point(0,64*facingId), true)

    member __.InitAnimation (animationId: AnimationId) (textures: TextureKind seq) =
        let weapon =
            animationId.weapon
            |> Option.toList
            |> List.map (fun w -> Weapon w, loadedWeapons.[w])
        { animation = AnimationBank.Animation animationId
          animationId = animationId
          textures =
            textures
            |> Seq.map (fun kind -> kind, loadedTextures.[ { animationKind = animationId.animationKind; textureKind = kind } ])
            |> Seq.append <| weapon
            |> Seq.toArray }

    member this.InitAnimationAndLoadTextures (animationId: AnimationId) (textures: TextureKind array) =
        if loadedWeapons |> Map.isEmpty then
            this.LoadWeapons()
        for ak in AnimationKind.AllTextureNames do
          for texture in textures do
            this.LoadTexture { TextureId.animationKind = ak; TextureId.textureKind = texture}
            |> ignore
        let weapon =
            animationId.weapon
            |> Option.toList
            |> List.map (fun w -> Weapon w, loadedWeapons.[w])
        { animation = AnimationBank.Animation animationId
          animationId = animationId
          textures =
            textures
            |> Seq.map (fun kind -> kind, loadedTextures.[ { animationKind = animationId.animationKind; textureKind = kind } ])
            |> Seq.append <| weapon
            |> Seq.toArray }