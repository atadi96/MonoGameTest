namespace MonoGameTest

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open MonoGame.Extended
open Wizard
open Tiles
open RigidBody
open Camera
open Sprites
open Interactables
open Level
open AnimationBank
open MonoGameExtensions.InputState
open SharpTileMap
open Element
open SpellBoulder

type Game1 () as this =
    inherit Game()
 
    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1280, PreferredBackBufferHeight = 720)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable wizard1 = Unchecked.defaultof<Wizard>
    let mutable wizard2 = Unchecked.defaultof<Wizard>
    let mutable camera = Unchecked.defaultof<Camera>
    let mutable elementsPosition = Unchecked.defaultof<Vector2>
    let mutable inputState = Unchecked.defaultof<InputState>
    let mutable bananaMap = Unchecked.defaultof<Sharptile.Map>
    //let mutable bananaTileLayers = Unchecked.defaultof<TileLayer list>
    //let mutable rigidBodies = Unchecked.defaultof<Map<string,RigidBody>>
    let mutable indoor1 = Unchecked.defaultof<Texture2D>
    let mutable indoor1TileSet = Unchecked.defaultof<TileSet>
    let mutable boulderCollection = Unchecked.defaultof<BoulderCollection>
    //let mutable interactables = Unchecked.defaultof<InteractableCollection>
    let mutable animationBank = Unchecked.defaultof<AnimationBank>
    let mutable level = Unchecked.defaultof<Level>
    let gravity = Vector2(0.f, 50.f)
    let maxSpeed = 350.f

    let getMovementVectorFromMouse(character: AABB, camera: Camera, mouseState: MouseState) =
        let mousePosition = Vector2(float32 mouseState.Position.X, float32 mouseState.Position.Y)
        let mouseVec2 = Vector2.Transform(mousePosition, camera.ScreenToWorld)
        let center = character.center
        let direction =
            mouseVec2 - center
            |> Vector2.Normalize
        let facing =
            match atan2 direction.Y direction.X with
            | x when x < - 3.f * MathHelper.PiOver4 -> LeftFacing
            | x when x < -MathHelper.PiOver4 -> UpFacing
            | x when x < MathHelper.PiOver4 -> RightFacing
            | x when x < 3.f * MathHelper.PiOver4 -> DownFacing
            | _ -> LeftFacing
        let isMoving =
            mouseState.LeftButton = ButtonState.Pressed &&
            direction.Length() > (Single.Epsilon * 10000.f) &&
            Vector2.Clamp(mouseVec2, character.min, character.max) <> mouseVec2
        //printfn "mousePosition = %A; mouseVec2 = %A; center = %A; direction = %A; Vector2.Clamp(%A, %A, %A) <> %A" mousePosition mouseVec2 center direction  mouseVec2 character.min character.max mouseVec2
        if isMoving then
            direction, isMoving, facing
        else
            Vector2.Zero, isMoving, facing

    let mouseWorldTarget(camera: Camera, mouseState: MouseState) =
        let mousePosition = Vector2(float32 mouseState.Position.X, float32 mouseState.Position.Y)
        let mouseVec2 = Vector2.Transform(mousePosition, camera.ScreenToWorld)
        mouseVec2
            
    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize() =
        camera <- Camera(this.GraphicsDevice.Viewport)
        camera.Zoom <- 2.f
        indoor1 <- this.Content.Load<Texture2D>("tilesets/rpg indoor tileset expansion 1 trans")
        indoor1TileSet <- TileSet.createTileSet(11, 12, 32, 32, indoor1)
        inputState <- InputState.init()
        boulderCollection <- BoulderCollection.empty
        //camera.Center(Vector2(float32 (tileLayer.width * tileSet.tileWidth) / 2.f, float32 (tileLayer.height * tileSet.tileHeight) / 2.f))
        //camera.Center(Vector2(float32 (tileLayer.width * tileSet.tileWidth) / 2.f, float32 (tileLayer.height * tileSet.tileHeight) / 2.f))
        base.Initialize()

    override this.LoadContent() =
        animationBank <- AnimationBank("character_animation", this.Content)
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        let contentPath = System.IO.Path.Combine(System.IO.Path.Combine(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location), this.Content.RootDirectory))
        bananaMap <- Newtonsoft.Json.JsonConvert.DeserializeObject<Sharptile.Map>(System.IO.File.ReadAllText(System.IO.Path.Combine(contentPath, "banana.json")))
        // bananaTileLayers <- bananaMap |> SharpTileMap.tileLayers
        // rigidBodies <- bananaMap |> SharpTileMap.rigidBodies //|> Map.filter (fun n _ -> n.StartsWith("wizard"))
        //let spawn = rigidBodies.["spawn"].aabb.center
        // interactables <- bananaMap |> SharpTileMap.interactables
        level <- Level.createFrom bananaMap
        
        let wizardLayers = [| Body Male; Head RobeHood; Torso RobeShirtBrown; Legs RobeSkirt |]

        wizard1 <-
          { name = "wizard1"
            bankAnimation = animationBank.InitAnimationAndLoadTextures (AnimationId.create DownFacing Idle) wizardLayers
            bodyLayers = wizardLayers
            color = Color.Green//Color(80,80,255,255)
            speed = 100.f
            elements = ElementCollection.empty
            walking = false
            rigidBody = level.mapRigidBodies.["wizard1"]
            channelStartTotalSeconds = None
            health = Health.create 1000 |> Health.add -700 }

        wizard2 <-
          { name = "wizard2"
            bankAnimation = animationBank.InitAnimationAndLoadTextures (AnimationId.create DownFacing Idle) wizardLayers
            bodyLayers = wizardLayers
            color = Color(200, 40, 40)
            elements = ElementCollection.empty
            speed = 100.f
            walking = false
            rigidBody = level.mapRigidBodies.["wizard2"]
            channelStartTotalSeconds = None
            health = Health.create 1000 }//RigidBody.create(rigidBody.mass, 32.f, 48.f, rigidBody.aabb.center + Vector2(50.f, 0.f), Vector2.Zero) }

        level <-
           { level with
               mapRigidBodies = level.mapRigidBodies |> Map.remove "wizard1" |> Map.remove "wizard2"
           }

        camera.Center(wizard1.rigidBody.aabb.center)
    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit();
        //printfn "Wizard = %A; Camera = %A" wizard1.rigidBody.aabb.center camera.Position

        inputState <- inputState |> InputState.update

        let dt = float32 gameTime.ElapsedGameTime.TotalSeconds
        if dt > 0.f then
            let friction = 0.4f

            let rigidBodies, contactReports =
                level.mapRigidBodies
                |> fun mrb ->
                    level.interactables.Items
                    |> List.fold (fun mrb inter -> mrb |> Map.add inter.name inter.rigidBody) mrb
                |> fun rbs ->
                    boulderCollection
                    |> BoulderCollection.rigidBodies
                    |> Seq.fold (fun rbs (name,rb) -> rbs |> Map.add name rb) rbs
                |> Map.add "wizard1" wizard1.rigidBody
                |> Map.add "wizard2" wizard2.rigidBody
                |> Physics2.SAT.collideSystem friction dt

            let boulderCollection', boulderHurtIntents =
                boulderCollection
                |> BoulderCollection.update gameTime rigidBodies contactReports

            let rigidBodies =
                rigidBodies
                |> Map.map (fun _ rb ->
                    { rb with
                        aabb = { rb.aabb with center = rb.aabb.center + rb.velocity * dt }
                    }
                )

            let hurtResult =
                seq {
                    yield wizard1 |> Wizard.hurtIntent gameTime
                    yield wizard2 |> Wizard.hurtIntent gameTime
                    yield! level.interactables.HurtIntents
                    yield! boulderHurtIntents
                }
                |> Seq.toArray
                |> HurtIntent.HurtSystem.calculateHurtResults

            let getHurtResult name =
                match hurtResult.hurtList |> Map.tryFind name with
                | Some x -> x
                | None -> []

            let wizard1Update =
                wizard1
                |> Wizard.updateWizard
                    friction
                    gameTime
                    (mouseWorldTarget(camera, inputState.currMouse))
                    (Wizard.getInput wizard1 inputState)
                    (getHurtResult wizard1.name)
            let wizard2Update =
                wizard2
                |> Wizard.updateWizard
                    friction
                    gameTime
                    wizard2.rigidBody.aabb.center
                    WizardInput.empty
                    (getHurtResult wizard2.name)
            match wizard1Update.spawnBoulder with
            | None -> ()
            | Some boulderData -> printfn "DIRR! %A" boulderData

            wizard1 <-
              let rb = rigidBodies.[wizard1.name]
              { wizard1 with
                  rigidBody = { rb with velocity = rb.velocity + wizard1Update.movementVelocity }
                  bankAnimation = wizard1 |> Wizard.updateAnimation animationBank wizard1Update.animationId gameTime
                  walking = wizard1Update.walking
                  elements = wizard1Update.elements
                  health = wizard1.health.add wizard1Update.heal
                  channelStartTotalSeconds = wizard1Update.channelStartTotalSeconds }
            elementsPosition <- Vector2(wizard1.rigidBody.aabb.center.X, wizard1.rigidBody.aabb.center.Y + wizard1.rigidBody.aabb.halfExtents.Y)
            wizard2 <-
              let rb = rigidBodies.[wizard2.name]
              { wizard2 with
                  rigidBody = { rb with velocity = rb.velocity + wizard2Update.movementVelocity }
                  bankAnimation = wizard2 |> Wizard.updateAnimation animationBank wizard2Update.animationId gameTime
                  walking = wizard2Update.walking
                  elements = wizard2Update.elements
                  health = wizard2.health.add wizard2Update.heal
                  channelStartTotalSeconds = wizard2Update.channelStartTotalSeconds }

            let addBoulder (wizard: Wizard) (wizardUpdate: WizardUpdate) (boulderCollection: BoulderCollection) =
                match wizardUpdate.spawnBoulder with
                | None -> boulderCollection
                | Some data ->
                    boulderCollection
                    |> BoulderCollection.add wizard.name wizard.rigidBody.aabb data.direction data.castCompleteness gameTime data.earthCount data.hurtMediums

            let boulderCollection' = boulderCollection' |> addBoulder wizard1 wizard1Update
            let boulderCollection' = boulderCollection' |> addBoulder wizard2 wizard2Update

            boulderCollection <- boulderCollection'

            camera.Update(wizard1.rigidBody.aabb.center, gameTime)

            let interactionEvents = level.interactables.Update camera inputState wizard1.rigidBody.aabb rigidBodies hurtResult.hurtList
            level.interactables.ProcessEvents interactionEvents
            for event in interactionEvents do
                match event with
                | PickedUp "startbook" ->
                    printfn "STORY: Nooow, you're journey is starting! ...as in, if you manage to get out that door."
                | Read text -> printfn "STORY: %s" text
                | _ -> ()
            level <- level |> Level.update interactionEvents
    
        base.Update(gameTime)
 
    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp, transformMatrix = Nullable.op_Implicit camera.WorldToScreen)
        //TileLayer.draw(spriteBatch, tileSet, camera, tileLayer, this)
        //SharpTileMap.draw bananaMap spriteBatch
        level.tileLayers
        |> Seq.iter (fun layer -> TileLayer.draw(spriteBatch, indoor1TileSet, camera, layer, this))
        level.interactables.Items
        |> Seq.iter (fun inter -> Interactable.draw inter indoor1TileSet spriteBatch)
        level.mapRigidBodies |> Map.toSeq |> Seq.iter (fun (_,rb)->
            let rect = Rectangle(rb.aabb.min.ToPoint(), rb.aabb.size.ToPoint()).ToRectangleF()
            spriteBatch.DrawRectangle(rect, Color.Purple)
        )
        level.dynamicTiles
        |> Seq.iter (fun lobj -> LevelObject.draw lobj indoor1TileSet spriteBatch)
        BoulderCollection.draw boulderCollection spriteBatch
        Wizard.draw wizard1 gameTime spriteBatch
        Wizard.draw wizard2 gameTime spriteBatch
        Element.draw elementsPosition wizard1.elements.Elements spriteBatch
        spriteBatch.End()

        // TODO: Add your drawing code here

        base.Draw(gameTime)