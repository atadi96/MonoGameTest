module Camera

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Camera(viewport: Viewport) =
    member val WorldToScreen = Matrix.Identity with get, set
    member val ScreenToWorld = Matrix.Identity with get, set
    member val Zoom = 1.0f with get, set
    member val Position = Vector2.Zero with get, set
    member val Rotation = 0.0f with get, set
    member val Chasing = true with get, set
    
    member this.Update(pos: Vector2, gameTime: GameTime) =
        let viewPortPercentage = 0.2f
        let maxDist = float32 (min viewport.Width viewport.Height) * viewPortPercentage
        let distVec = Vector2.Transform(pos, this.WorldToScreen) - Vector2.Transform(this.Position, this.WorldToScreen)
        let chasing = this.Chasing || distVec.LengthSquared() > (maxDist * maxDist) * 0.8f
        let speed =
            if this.Chasing then
                max
                   ( (90.f * distVec.LengthSquared()) /
                     (maxDist * maxDist))
                   45.f
                |> min 90.f
            else 0.f
        let transVector, chasing =
            if distVec.LengthSquared() < 25.f then
                Vector2.Zero, false
            else if chasing then
                (distVec |> Vector2.Normalize) * speed * float32 gameTime.ElapsedGameTime.TotalSeconds
                , true
            else
                Vector2.Zero, false
        this.Chasing <- chasing
        this.Position <- this.Position + transVector
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-this.Position,0.f)) *
            Matrix.CreateRotationZ(this.Rotation) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, this.Zoom)) *
            Matrix.CreateTranslation(Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert this.WorldToScreen
    member this.Center(pos: Vector2) =
        this.Position <- pos
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-pos,0.f)) *
            Matrix.CreateRotationZ(this.Rotation) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, this.Zoom)) *
            Matrix.CreateTranslation(Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert this.WorldToScreen