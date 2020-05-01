module Sprites    
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System

type Sprite =
    { position: Vector2; speed: float32; texture: Texture2D; size: Point; offset: Point}
    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRect = Rectangle(this.offset, this.size)
        spriteBatch.Draw(this.texture, this.position, Nullable.op_Implicit sourceRect, Color.White)

module AnimationFrames =
    let horizontalStrip(frameCount, size: Point, offset: Point) =
        [| for i in 0..frameCount-1 ->
            Rectangle(offset + Point(size.X * i, 0), size)
        |]

type Animation =
  { frames: Rectangle array
    fps: int
    currentFrame: int
    frameTimer: TimeSpan
    frameLength: TimeSpan
    size: Point
    repeat: bool }
    static member Create(frameCount,fps,size: Point, offset: Point, repeat: bool) =
        let frames = AnimationFrames.horizontalStrip(frameCount, size, offset)
        { frames = frames
          currentFrame = 0
          frameTimer = TimeSpan.Zero
          frameLength = TimeSpan.FromSeconds(float (1.f / float32 fps))
          fps = fps
          size = size
          repeat = repeat }

    member this.CurrentFrame = this.frames.[this.currentFrame]
    member this.IsOver =
        not this.repeat &&
        this.currentFrame + 1 = this.frames.Length &&
        this.frameTimer >= this.frameLength
    member this.SetLength (sec: float32) =
        let fps = float32 this.frames.Length / sec
        let frameLength = 1.f / fps
        { this with
            frameLength = TimeSpan(0, 0, 0, 0, int (frameLength * 1000.f))
            fps = int fps }

module Animation =
    let reset anim =
        { anim with  currentFrame = 0; frameTimer = TimeSpan.Zero}

    let update (gameTime: GameTime) (animation: Animation) =
        let newFrameTimer, newFrame =
            match animation.frameTimer + gameTime.ElapsedGameTime with
            | n when n >= animation.frameLength ->
                if animation.currentFrame + 1 >= animation.frames.Length && not animation.repeat then
                    n, animation.currentFrame
                else
                    TimeSpan.Zero, (animation.currentFrame + 1) % animation.frames.Length
            | n -> n, animation.currentFrame
        { animation with
            frameTimer = newFrameTimer
            currentFrame = newFrame}
