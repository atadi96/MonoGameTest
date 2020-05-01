module Level
open Tiles
open RigidBody
open Interactables
open SharpTileMap

type Level =
  { tileLayers: TileLayer list
    mapRigidBodies: Map<string,RigidBody>
    interactables: InteractableCollection
    dynamicTiles: LevelObject list }

module Level =
  let createFrom (map: Sharptile.Map) =
    { tileLayers = tileLayers map
      mapRigidBodies = rigidBodies map
      interactables = interactables map
      dynamicTiles = dynamicTiles map}

  let update (commands: InteractionEvent list) (level: Level) =
    //apply the commands to the dynamic tiles
    let dynamicTiles =
        commands
        |> List.fold (fun (lobjs: LevelObject list) command ->
            match command with
            | Broke "room1door" ->
                lobjs
                |> List.where (fun lobj -> lobj.name.StartsWith("room1door") |> not)
            | _ -> lobjs
        ) level.dynamicTiles
    { level with
        dynamicTiles = dynamicTiles
    }