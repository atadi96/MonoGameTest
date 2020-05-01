module Interactables
open RigidBody
open Camera
open Microsoft.Xna.Framework
open Tiles
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open MonoGameExtensions.InputState
open MonoGame.Extended

type Interactable =
  { name: string
    physicalBreak: int option
    tileId: int option
    scriptum: string list option
    pickup: string option
    rigidBody: RigidBody
    highlighted: ref<bool> }
  member this.isHighlighted = !this.highlighted
  member this.Update (camera: Camera) (mousePos: Point) =
    let mouseWorldPoint = Vector2.Transform(mousePos.ToVector2(), camera.ScreenToWorld).ToPoint()
    let boundingRectangle = Rectangle(this.rigidBody.aabb.min.ToPoint(), this.rigidBody.aabb.size.ToPoint())
    this.highlighted.Value <- boundingRectangle.Contains(mouseWorldPoint)

let (|Breakable|_|) (i: Interactable) = i.physicalBreak
let (|Readable|_|) (i: Interactable) = i.scriptum
let (|Pickable|_|) (i: Interactable) = i.pickup

module Interactable =
  let draw (inter: Interactable) (tileSet: TileSet) (sb: SpriteBatch) =
    let boundingRectangle = Rectangle(inter.rigidBody.aabb.min.ToPoint(), inter.rigidBody.aabb.size.ToPoint())
    sb.DrawRectangle(boundingRectangle.ToRectangleF(), Color.Indigo)
    match inter.tileId with
    | Some tileId ->
        sb.Draw(tileSet.texture, boundingRectangle, System.Nullable.op_Implicit tileSet.sourceRectangles.[tileId], if inter.isHighlighted then Color.LightSkyBlue else Color.White)
    | None when inter.isHighlighted ->
        sb.DrawRectangle(boundingRectangle.ToRectangleF(), Color.LightSkyBlue, 3.f)
    | None -> ()
    

  let inReach (from: AABB) (interactable: Interactable) =
    let aabb = interactable.rigidBody.aabb
    let delta = aabb.center - from.center
    let extents = aabb.halfExtents + from.halfExtents
    delta.Length() - abs extents.X < 4.f && (aabb.min.Y <= from.center.Y && from.center.Y <= aabb.max.Y) ||
    (delta.Length() - abs extents.Y < 4.f && (aabb.min.X <= from.center.X && from.center.X <= aabb.max.X))

type InteractionEvent =
  | Broke of string
  | PickedUp of string
  | Read of string

open HurtIntent

type InteractableCollection(items: Interactable list) =
    let mutable items = items
    let mutable currentlyApproachingItem = None : Interactable option

    let updateHighlight (camera: Camera) (mousePos: Point) (clickStart: bool) (items: Interactable list, currentlyApproachingItem: Interactable option) =
        for item in items do
            item.Update camera mousePos
        let currentlyApproachingItem =
            match items |> List.tryFind(fun i -> i.isHighlighted), clickStart, currentlyApproachingItem with
            | hi, true, _ -> hi
            | _, false, Some i when i.isHighlighted -> Some i
            | _ -> None
        items, currentlyApproachingItem

    let update (camera: Camera)
               (inputState: InputState)
               (wizardAABB: AABB)
               (rigidBodies: Map<string,RigidBody>)
               (hurtResponse: Map<string,HurtIntent list>)
               (items: Interactable list, currentlyApproachingItem: Interactable option) =
        let updateRigidBody i =
            match rigidBodies |> Map.tryFind i.name with
            | Some rb -> { i with rigidBody = rb }
            | None -> i
        let physicalDamage name =
            query {
              for hr in hurtResponse |> Map.tryFind name |> Option.toList do
              for hi in hr do
              for ai in hi.attackIntent |> Option.toList do
              select ai.mediums
            }
            |> HurtMediumMap.unionMany
            |> HurtMediumMap.tryMedium Physical
            
        let clickStart =
            match inputState with
            | MouseClick LeftMouseButton & KeyUp Keys.LeftShift -> true
            | _ -> false
        (items |> List.map (updateRigidBody), currentlyApproachingItem)
        |> updateHighlight camera inputState.MousePosition clickStart
        |> fun (items, currentItem) ->
            items
            |> List.mapFold (fun (events,highlighted) interactable ->
                match interactable, physicalDamage interactable.name, highlighted with
                | Breakable cap, Some dmg, _ when cap <= dmg ->
                    let event = Broke interactable.name
                    let highlighted =
                      match highlighted with
                      | Some i when i = interactable -> None
                      | _ -> highlighted
                    None, ((event :: events), highlighted)
                | Pickable pick, _, Some h when interactable = h && Interactable.inReach wizardAABB interactable ->
                    let event = PickedUp pick
                    None, (event :: events, None)
                | Readable scriptum, _, Some h when interactable = h && clickStart && Interactable.inReach wizardAABB interactable ->
                    let event, scriptum =
                      match scriptum with
                      | [ lastText ] -> Read lastText, [lastText]
                      | text :: rest -> Read text, rest
                      | [] -> Read "", []
                    Some { interactable with scriptum = Some scriptum }, (event::events, highlighted)
                | _ -> Some interactable, (events, highlighted)
            ) ([],currentItem)
        |> fun (a,(b,c)) -> a |> List.choose id, b, c
        
    member __.Items = items
    member __.SetState(items',curr) =
      items <- items'
      currentlyApproachingItem <- curr
    member __.HurtIntents =
      items
      |> Seq.choose (fun it ->
        match it.physicalBreak with
        | None -> None
        | Some _ -> Some (HurtIntent.create it.name it.rigidBody.aabb)
      )
    member this.Update (camera: Camera) (inputState: InputState) (wizardAABB: AABB) (rigidBodies: Map<string,RigidBody>) (hurtResponse: Map<string,HurtIntent list>) =
        let items, events, highlighted = update camera inputState wizardAABB rigidBodies hurtResponse (items, currentlyApproachingItem)
        this.SetState (items, highlighted)
        events
    member this.ProcessEvents (events: InteractionEvent list) =
        let items =
          items
          |> List.map (fun inter ->
              if inter.name = "room1door" && events |> List.contains (PickedUp "startbook") then
                { inter with
                    physicalBreak = Some 10
                    scriptum =
                      [ "It's closed, but if you give it a good swing, it might give in..."
                        "Come on, give a punch!"
                        "Ah, for Christ's sake, SHIFT + Left Click, facing the door... noob." ]
                      |> Some }
              else inter
          )
        this.SetState(items, currentlyApproachingItem)