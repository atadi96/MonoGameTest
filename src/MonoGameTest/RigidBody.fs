module RigidBody

open Microsoft.Xna.Framework

type AABB =
    { halfExtents: Vector2
      center: Vector2 }

    member this.min = this.center - this.halfExtents
    member this.max = this.center + this.halfExtents
    member this.size = this.halfExtents * 2.f
    member this.rectangle = Rectangle(this.min.ToPoint(), this.size.ToPoint())
    static member create(center, halfExtents) =
        { center = center; halfExtents = halfExtents }

type RigidBody =
    { mass: float32
      inverseMass: float32
      aabb: AABB
      velocity: Vector2
      onGround: bool
      onGroundLast: bool
      reportContact: bool }
     
    static member create(mass, width, height, center, vel) =
        { mass = mass
          inverseMass = if mass = 0.f then 0.f else 1.f / mass
          aabb = AABB.create(center, Vector2(width / 2.f, height / 2.f))
          velocity = vel
          onGround = false
          onGroundLast = false
          reportContact = false }