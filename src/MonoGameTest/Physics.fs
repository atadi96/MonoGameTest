module Physics
open RigidBody
open Microsoft.Xna.Framework
open MonoGame.Extended
open Tiles

type Contact =
    { a: RigidBody
      b: RigidBody
      normal: Vector2
      distance: float32
      impulse: float32 }

    static member create(a, b, normal, dist, ?impulse) =
        { a = a
          b = b
          normal = normal
          distance = dist
          impulse = Option.defaultValue 0.f impulse }      

module Speculative =
    let speculativeSolver (dt: float32) (contact: Contact) =

        let normal = -contact.normal

        //get all the relative normal velocity
        let nv = Vector2.Dot(contact.b.velocity - contact.a.velocity, normal)
        if nv > 0.f then
            contact
        else
            //remove enough velocity to leave them just touching
            let remove = nv + (contact.distance / dt)

            //compute impulse
            let impulse = remove / (contact.a.inverseMass + contact.b.inverseMass)

            //accumulate
            let newImpulse = min (impulse + contact.impulse) 0.f

            //compute change
            let change = newImpulse - contact.impulse

            //store accumulated impulse and apply impulse
            { contact with
                a = {contact.a with
                        velocity = contact.a.velocity + change * normal * contact.a.inverseMass}
                b = {contact.b with
                        velocity = contact.b.velocity - change * normal * contact.b.inverseMass}
                impulse = newImpulse }

module Collision =

    let isInternalCollision (tileX: int) (tileY: int) (normal: Vector2) (tileLayer: TileLayer) =
        let nextTileX = tileX + int normal.X
        let nextTileY = tileY + int normal.Y

        let currentTile = TileLayer.getTileId tileX tileY tileLayer
        let nextTile = TileLayer.getTileId nextTileX nextTileY tileLayer

        match nextTile with
        | None -> false
        | Some _ -> true

    let AABBVsAABB (a: RigidBody) (b: RigidBody) tileX tileY (map: TileLayer) =
        let combinedExtents = b.aabb.halfExtents + a. aabb.halfExtents
        let delta = b.aabb.center - a.aabb.center

        let normal = delta.MajorAxis() |> Vector2.Negate

        let planeCentre = (normal * combinedExtents) + b.aabb.center
        let planeDelta = a.aabb.center - planeCentre
        let dist = planeDelta.Dot normal
        let contact = Contact.create(a,b,normal,dist)
        let internalCollision = isInternalCollision tileX tileY normal map
        not internalCollision, contact

    let collisionResponse (movableObject: RigidBody) (other: RigidBody) (contact: Contact) (dt: float32) =
        let friction = 0.4f
        let solved = Speculative.speculativeSolver dt contact
        failwith "i lost it completely. I just lost it "

    let innerCollide (tileLayer: TileLayer) (movableObject: RigidBody) (tileAabb: AABB) (tileType: int option) (dt: float32) (x: int) (y: int) =
        match tileType with
        | None -> None
        | tileType ->
            let tileRigidBody = RigidBody.create(0.f, tileAabb.size.X, tileAabb.size.Y, tileAabb.center, Vector2.Zero)
            let collision, contact = AABBVsAABB movableObject tileRigidBody x y tileLayer

            if collision then None
            else None

    let collision (map: TileLayer) (tileSet: TileSet) (rigidBody: RigidBody) (dt: float32) =

        let expand = Vector2(5.f, 5.f)
        let predictedPos = rigidBody.aabb.center + (rigidBody.velocity * dt)
        let min = Vector2.Min(rigidBody.aabb.center, predictedPos) - rigidBody.aabb.halfExtents - expand
        let max = Vector2.Max(rigidBody.aabb.center, predictedPos) + rigidBody.aabb.halfExtents + expand

        TileLayer.getBroadphaseTiles map tileSet min max
        |> List.choose(fun (tileId, tileaabb, x, y) -> None)