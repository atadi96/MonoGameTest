module Physics2
open RigidBody
open Microsoft.Xna.Framework
open MonoGame.Extended
open Tiles

type ContactReport =
    { requester: RigidBody
      subjectName: string
      subjectRigidBody: RigidBody }

module ContactReport =
    let create r n rb = { requester = r; subjectName = n; subjectRigidBody = rb }

module SAT =
    let sweep (dt: float32) (rb: RigidBody) =
        let min = Vector2.Min(rb.aabb.min + rb.velocity * dt, rb.aabb.min)
        let max = Vector2.Max(rb.aabb.max + rb.velocity * dt, rb.aabb.max)
        let halfExtents = (max - min) / 2.f
        AABB.create (min + halfExtents, halfExtents)
    let sweep' (dt: float32) (rb: RigidBody) =
        AABB.create (rb.aabb.center + rb.velocity * dt, rb.aabb.halfExtents)
    let mtv' (dt: float32) (a: RigidBody) (b: RigidBody) =
        if a.inverseMass + b.inverseMass = 0.f then None else
        let sweepA = sweep dt a
        let sweepB = sweep dt b
        let intersect =
            Rectangle.Intersect
              ( Rectangle(sweepA.min.ToPoint(), sweepA.size.ToPoint())
              , Rectangle(sweepB.min.ToPoint(), sweepB.size.ToPoint())
              )
        let relVelA =
            let v = a.velocity - b.velocity
            if v = Vector2.Zero then
                b.aabb.center - a.aabb.center
            else v
        if intersect.Size <> Point.Zero then
            let mtvs =
                [ 0,1
                  1,0
                  0,-1
                  -1,0 ]
                |> Seq.map (fun (x,y) -> Vector2(float32 x, float32 y))
                |> Seq.filter (fun axis ->
                    axis.Dot relVelA < 0.f || relVelA.Length() = 0.f
                )
                |> Seq.map (fun normal ->
                    normal * (intersect.Size.ToVector2())
                )
                |> Seq.toList
                |> List.sortBy (fun tv -> tv.LengthSquared())
            let mtv =
                match mtvs with
                | a :: b :: _ ->
                    let da = a.LengthSquared()
                    let db = b.LengthSquared()
                    if abs (da - db) < 10.f then
                        // if the two normals don't differ more than 10% in length,
                        // we choose the one that "throws off" the object less
                        // which means that we choose the one which has a smaller
                        // dot product with the "closer" perpendicular of the relative speed
                        // closer means if the normal goes left from the relative speed then
                        // the left perpendicular etc
                        (*
                        let perpendicularFor (asix: Vector2) =
                            // idk don't ask
                            match asix.X, asix.Y with
                            | 0.f, _ -> Vector2(relVelA.Y, -relVelA.X)
                            | _, 0.f -> Vector2(-relVelA.Y, relVelA.X)
                            | _ -> failwith "no"
                        if a.Dot(perpendicularFor a) < b.Dot(perpendicularFor b) then
                           a
                        else b *)
                        a + b
                    else
                        a
                | mtv :: _ -> mtv
                | [] -> failwith "can't happen"
            let mm = a.inverseMass + b.inverseMass
            let mm = mm * 2.f
            Some (mtv * a.inverseMass / mm, -mtv * b.inverseMass / mm)
        else
            None
    let mtv (dt: float32) (a: RigidBody) (b: RigidBody) =
        if a.inverseMass + b.inverseMass = 0.f then None else
        let sweepA = sweep dt a
        let sweepB = sweep dt b
        let intersect =
            Rectangle.Intersect
              ( Rectangle(sweepA.min.ToPoint(), sweepA.size.ToPoint())
              , Rectangle(sweepB.min.ToPoint(), sweepB.size.ToPoint())
              )
        let relVelA =
            let v = a.velocity - b.velocity
            if v = Vector2.Zero then
                b.aabb.center - a.aabb.center
            else v
        if intersect.Size <> Point.Zero then
            let mtv =
                [ 0,1
                  1,0
                  0,-1
                  -1,0 ]
                |> Seq.map (fun (x,y) -> Vector2(float32 x, float32 y))
                |> Seq.filter (fun axis ->
                    axis.Dot relVelA < 0.f || relVelA.LengthSquared() = 0.f
                )
                |> Seq.map (fun normal ->
                    normal * (intersect.Size.ToVector2())
                )
                |> Seq.minBy (fun n -> n.LengthSquared())
                //|> Seq.fold (fun (minv: Vector2) curr -> if curr.LengthSquared() < minv.LengthSquared() - 0.001f then curr else minv) (intersect.Size.ToVector2())
            let mm = a.inverseMass + b.inverseMass
            Some (mtv * a.inverseMass / mm, -mtv * b.inverseMass / mm)
        else
            None

    let collideSystem (friction: float32) (dt: float32) (system: Map<string,RigidBody>) =
        let names = system |> Map.toSeq |> Seq.map fst |> Seq.toArray
        let mutable system = system
        let mutable reports = Map.empty<string, ContactReport list>
        for _iteration in 1..1 do
            for i in 0..names.Length-1 do
              for j in i+1..names.Length-1 do
                let nameA = names.[i]
                let nameB = names.[j]
                let a = system.[nameA]
                let b = system.[nameB]
                let system', reports' =
                    match mtv dt a b with
                    | None -> system, reports
                    | Some (mtva,mtvb) ->
                        let update rb mtv =
                            { rb with
                                aabb = { rb.aabb with center = rb.aabb.center + mtv }
                            }
                        let system' =
                            system
                            |> Map.add nameA (update a mtva)
                            |> Map.add nameB (update b mtvb)
                        let reports =
                            if a.reportContact then
                                reports |> Map.addToList nameA (ContactReport.create a nameB b)
                            else reports
                            |> fun reports ->
                                if b.reportContact then
                                    reports |> Map.addToList nameB (ContactReport.create b nameA a)
                                else reports
                        system', reports
                system <- system'
                reports <- reports'
        system
        |> Map.map (fun _ rb ->
            { rb with velocity = rb.velocity * (1.f - friction) }
        ), reports
