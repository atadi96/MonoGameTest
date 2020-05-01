module SpellBoulder
open RigidBody
open HurtIntent
open Microsoft.Xna.Framework
open Physics2
open Microsoft.Xna.Framework.Graphics

type SpellBoulder =
  { channelMultiplier: float32
    castCompleteness: float32
    freshlySpawned: bool
    name: string
    owner: string
    flyingSpeed: float32
    landTime: System.TimeSpan
    disappearTime: System.TimeSpan
    rigidBody: RigidBody
    mediums: HurtMediumMap
    spawnTime: System.TimeSpan }

module SpellBoulder =
    let hurtIntent (sb: SpellBoulder) =
        let ignoreList =
            if sb.freshlySpawned then
                []//[ sb.owner ]
            else []
        let hi =
            HurtIntent.createWithMediumMap
                sb.name
                sb.rigidBody.aabb
                sb.rigidBody.aabb
                (sb.mediums |> HurtMediumMap.multiplyFloat32 Physical sb.channelMultiplier)
        { hi with ignores = ignoreList }

    let create (name: string) (owner: string) (ownerAABB: AABB) (direction: Vector2) (castCompleteness: float32) (gameTime: GameTime) (earthCount: int) (attackMediums: HurtMediumMap): SpellBoulder =
        let direction = direction |> Vector2.Normalize
        let ratio (minmax: {| min: float32; max: float32 |}) ratio =
            minmax.min + (minmax.max - minmax.min) * ratio
        let channelBonus = ratio {| min = 1.0f; max = 5.0f |} castCompleteness
        let speed = ratio {| min = 50.f; max = 200.f |} castCompleteness
        let speed = ratio {| min = 100.f; max = 100.f |} castCompleteness
        if earthCount = 0 then failwith "must have earth element to cast boulder"
        let earthRatio = (float32 earthCount - 1.f) / 4.f
        let mass = ratio {| min = 10.f; max = 180.f|} earthRatio
        let size = ratio {| min = 5.f; max = 32.f |} earthRatio
        let landTime =
            let seconds = ratio {| min = 1.f / 6.f; max = 0.5f |} castCompleteness
            let seconds = ratio {| min = 0.5f; max = 0.5f |} castCompleteness
            let ts = System.TimeSpan.FromSeconds (float seconds)
            gameTime.TotalGameTime.Add ts
        let disappearTime = landTime.Add(System.TimeSpan.FromSeconds(0.5))
        let signDirection = Vector2(direction.X |> sign |> float32, direction.Y |> sign |> float32)
        let center =
            let lambda = min (ownerAABB.halfExtents.Y / 2.f / direction.X) (ownerAABB.halfExtents.X / 2.f / direction.Y)
            let lambda' = min (size / 4.f / direction.X) (size / 4.f / direction.Y)
            let ownerIntersection = lambda * direction
            let boulderIntersection = lambda' * direction
            ownerAABB.center - ownerIntersection - boulderIntersection
        { channelMultiplier = channelBonus
          freshlySpawned = true
          name = name
          owner = owner
          rigidBody = { RigidBody.create (mass, size, size, center, (direction * speed)) with reportContact = true }
          flyingSpeed = speed
          castCompleteness = castCompleteness
          landTime = landTime
          disappearTime = disappearTime
          mediums = attackMediums
          spawnTime = gameTime.TotalGameTime }

    let update (gameTime: GameTime) (rigidBody: RigidBody) (contacts: ContactReport list) (boulder: SpellBoulder) =
        // ha frissen spawnolt de a kontaktokban nincs benne az owner, vagy van más az owneren kívül, akkor már nem frissen spawnolt
        // ha nem frissen spawnlot, akkor minden kontakja kapja a hurtintentet az arcocskájába
        // ha még nem telt el adott idő a dobás elejétől kezdve a cast kitartásától függően, akkor a sebesség fixen a kezdeti sebesség, mindegy a súrlódás
        // if it's freshly spawned but the owner isn't in the contacts or there's anything else in the contacts, then it's not freshly spawned anymore
        // if it's note freshly spawned then a HurtIntent shall be output for every contact
        // when less time has passed since spawn than a given time influenced by the channeling length, the initial speed is kept, overriding the friction
        if gameTime.TotalGameTime >= boulder.disappearTime then None else
        let freshlySpawned =
            if boulder.freshlySpawned then
                match contacts with
                | [ { subjectName = sn } ] -> sn = boulder.owner
                | _ -> false
            else false
        let velocity = 
            if gameTime.TotalGameTime < boulder.landTime then
                boulder.flyingSpeed * (boulder.rigidBody.velocity |> Vector2.NormalizeOrZero)
            else boulder.rigidBody.velocity
        let hurtIntents =
            if freshlySpawned || velocity.LengthSquared() < 0.001f then [] else
            contacts
            |> List.map (fun contact ->
                HurtIntent.createWithMediumMap
                    boulder.name
                    rigidBody.aabb
                    contact.subjectRigidBody.aabb
                    (boulder.mediums |> HurtMediumMap.multiplyFloat32 Physical boulder.channelMultiplier)
            )
        ( { boulder with
              freshlySpawned = freshlySpawned
              rigidBody =
                { rigidBody with velocity = velocity }
            }
        , hurtIntents)
        |> Some

type BoulderCollection = private | BC of (int * SpellBoulder list)
    with
    member private this.Boulders = match this with BC x -> snd x

module BoulderCollection =
    let empty = BC (0,[])

    let update (gameTime: GameTime) (rigidBodies: Map<string,RigidBody>) (contacts: Map<string,ContactReport list>) (BC (id,bc): BoulderCollection) =
        let mutable boulders = []
        let mutable hurtIntents = []
        for boulder in bc do
            let boulder' =
                boulder
                |> SpellBoulder.update
                    gameTime
                    rigidBodies.[boulder.name]
                    (contacts |> Map.tryFind boulder.name |> Option.defaultValue [])
            match boulder' with
            | None -> ()
            | Some (boulder',his) ->
                boulders <- boulder' :: boulders
                for hi in his do
                    hurtIntents <- hi :: hurtIntents
        BC (id, boulders), hurtIntents

    let add (owner: string) (ownerAABB: AABB) (direction: Vector2) (castCompleteness: float32) (gameTime: GameTime) (earthCount: int) (attackMediums: HurtMediumMap) (BC (id,bc)) =
        let boulder =
            SpellBoulder.create
                (sprintf "___spellboulder%iname" id)
                owner
                ownerAABB
                direction
                castCompleteness
                gameTime
                earthCount
                attackMediums
        printfn "%A" boulder
        BC (id+1, boulder::bc)

    let rigidBodies (BC (_,bc)) =
        bc
        |> Seq.map (fun bc -> bc.name, bc.rigidBody)

    let draw (BC (_,bc)) (spriteBatch: SpriteBatch) =
        for boulder in bc do
            let circle r =
                let diam = r / 2
                let diamsq = diam * diam
                let texture = new Texture2D(spriteBatch.GraphicsDevice, r, r);
                [|  for x in 0..r-1 do
                      for y in 0..r-1 do
                        if Vector2(float32 (x - diam), float32 (y - diam)).LengthSquared() <= float32 diamsq then
                            yield Color.White
                        else
                            yield Color.Transparent
                |]
                |> texture.SetData
                texture
            let circleTexture = circle 100
                
            spriteBatch.Draw(circleTexture, boulder.rigidBody.aabb.rectangle, Color.Brown)
