module SharpTileMap

open Tiles
open RigidBody
open Sharptile
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open Camera
open Interactables

type LevelObject =
  { name: string
    rigidBody: RigidBody
    tileId: int option }

module LevelObject =
  let draw (levelObj: LevelObject) (tileSet: TileSet) (sb: SpriteBatch) =
    let boundingRectangle = Rectangle(levelObj.rigidBody.aabb.min.ToPoint(), levelObj.rigidBody.aabb.size.ToPoint())
    //sb.Draw(tileSet.texture, boundingRectangle, System.Nullable.op_Implicit tileSet.sourceRectangles.[inter.tileId], if inter.isHighlighted then Color.LightSkyBlue else Color.White)
    match levelObj.tileId with
    | Some tileId ->
        sb.Draw(tileSet.texture, boundingRectangle, System.Nullable.op_Implicit tileSet.sourceRectangles.[tileId], Color.White)
    | None -> ()

let tileLayers (map: Map) =
    map.layers
    |> Seq.filter (fun layer -> layer.``type`` = "tilelayer")
    |> Seq.map (fun layer ->
        { tiles = layer.data
          width = layer.width
          height = layer.height
          visible = layer.visible
        }
    )
    |> Seq.toList

let noNull (xs: _ array) =
  match xs with
  | null -> [||]
  | _ -> xs

let rigidBodies (map: Map) =
    map.layers
    |> Seq.filter (fun layer ->layer.``type`` = "objectgroup" && layer.name = "map")
    |> Seq.collect (fun layer ->
        layer.objects
        |> Seq.map (fun obj ->
            obj.name,
            RigidBody.create
              ( (noNull obj.properties) |> Seq.tryFind (fun p -> p.name = "mass") |> Option.map (fun p -> float32 p.value) |> Option.defaultValue 0.f
              , float32 obj.width// * float32 tileSet.tileWidth
              , float32 obj.height// * float32 tileSet.tileHeight
              , Vector2
                ( float32 (obj.x + obj.width / 2.)// * float32 tileSet.tileWidth
                , float32 (obj.y + obj.height / 2.)// * float32 tileSet.tileHeight
                )
              , Vector2.Zero
              )
        )
    )
    |> Map.ofSeq

let interactables (map: Map) =
    map.layers
    |> Seq.filter (fun layer ->layer.``type`` = "objectgroup" && layer.name = "interactables")
    |> Seq.collect (fun layer ->
        layer.objects
        |> Seq.map (fun obj ->
            let tileId = Some obj.gid |> Option.filter (fun i -> i > 0) |> Option.map (fun i -> i - 1)
            let properties = obj.properties |> Option.ofObj |> Option.defaultValue [||]
            let centerOffset =
              if tileId.IsSome then - obj.height / 2. else obj.height / 2.
            let getProp (parse: string -> 'T) name =
              properties |> Seq.tryFind (fun p -> p.name = name) |> Option.map (fun p -> parse p.value)
            { name = obj.name
              tileId = tileId
              physicalBreak = getProp int "physicalbreak"
              scriptum =
                getProp string "scriptum"
                |> Option.map (fun text ->
                  System.Text.RegularExpressions.Regex.Split(text, "\r\n|\r|\n")
                  |> Array.toList
                )
              pickup = getProp string "pickup" |> Option.map (fun item -> if System.String.IsNullOrWhiteSpace(item) then obj.name else item)
              rigidBody =
                RigidBody.create
                  ( getProp float32 "mass" |> Option.defaultValue 0.f
                  , float32 obj.width// * float32 tileSet.tileWidth
                  , float32 obj.height// * float32 tileSet.tileHeight
                  , Vector2
                    ( float32 (obj.x + obj.width / 2.)// * float32 tileSet.tileWidth
                    , float32 (obj.y + centerOffset)// * float32 tileSet.tileHeight
                    )
                  , Vector2.Zero
                  )
              highlighted = ref false
            }              
      )
    )
    |> List.ofSeq
    |> InteractableCollection

let dynamicTiles (map: Map) =
    map.layers
    |> Seq.filter (fun layer ->layer.``type`` = "objectgroup" && layer.name = "dynamicmap")
    |> Seq.collect (fun layer ->
        layer.objects
        |> Seq.map (fun obj ->
            let tileId = Some obj.gid |> Option.filter (fun i -> i > 0) |> Option.map (fun i -> i - 1)
            let properties = obj.properties |> Option.ofObj |> Option.defaultValue [||]
            let centerOffset =
              if tileId.IsSome then - obj.height / 2. else obj.height / 2.
            { name = obj.name
              tileId = tileId
              rigidBody =
                RigidBody.create
                  ( properties |> Seq.tryFind (fun p -> p.name = "mass") |> Option.map (fun p -> float32 p.value) |> Option.defaultValue 0.f
                  , float32 obj.width// * float32 tileSet.tileWidth
                  , float32 obj.height// * float32 tileSet.tileHeight
                  , Vector2
                    ( float32 (obj.x + obj.width / 2.)// * float32 tileSet.tileWidth
                    , float32 (obj.y + centerOffset)// * float32 tileSet.tileHeight
                    )
                  , Vector2.Zero
                  )
            }     
      )
    )
    |> List.ofSeq