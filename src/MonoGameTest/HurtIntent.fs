module HurtIntent
open RigidBody
open Microsoft.Xna.Framework
open Element

type HurtMedium =
    | Physical
    | Elemental of Element

type HurtMediumMap = private HMM of Map<HurtMedium,int> with
  member private this.Map = match this with HMM m -> m
  member this.Medium (medium: HurtMedium) = this.Map |> Map.tryFind medium
  member this.MediumDefault (medium: HurtMedium) = this.Map |> Map.tryFind medium |> Option.defaultValue 0

module HurtMediumMap =
  let ofSeq (ms: seq<HurtMedium * int>) =
    ms
    |> Seq.fold (fun hms (m,i) ->
      hms |> Map.addMonoid 0 (+) m i
    ) Map.empty
    |> HMM

  let toSeq (HMM hmm) = hmm |> Map.toSeq

  let add medium value (HMM hms) =
    hms
    |> Map.addMonoid 0 (+) medium value
    |> HMM

  let multiply medium value (HMM hms) =
    hms
    |> Map.tryFind medium
    |> function
      | None -> hms
      | Some v -> hms |> Map.add medium (v * value)
    |> HMM

  let multiplyFloat32 medium value (HMM hms) =
    hms
    |> Map.tryFind medium
    |> function
      | None -> hms
      | Some v -> hms |> Map.add medium (int (float32 v * value))
    |> HMM

  let unionMany (hmms: HurtMediumMap seq) =
    hmms
    |> Seq.collect (toSeq)
    |> ofSeq

  let tryMedium k (HMM hmm) = hmm |> Map.tryFind k

type AttackIntent =
  { aabb: AABB
    mediums: HurtMediumMap }
    member this.rectangle = this.aabb.rectangle
    member this.Medium (medium: HurtMedium) = this.mediums.Medium medium
    member this.MediumDefault (medium: HurtMedium) =
        medium
        |> this.Medium
        |> Option.defaultValue 0
    static member create (aabb: AABB) (mediums: HurtMediumMap) =
      { aabb = aabb; mediums = mediums}


type HurtIntent =
    { owner: string
      ignores: string list
      attackIntent: AttackIntent option
      ownAABB: AABB }
    static member create (owner: string) (ownAABB: AABB) =
      { owner = owner
        ignores = []
        ownAABB = ownAABB
        attackIntent = None }
    static member createSimple (owner: string) (ownAABB: AABB) (attackAABB: AABB) (mediums: seq<HurtMedium * int>) =
      { owner = owner
        ignores = []
        ownAABB = ownAABB
        attackIntent = Some (AttackIntent.create attackAABB (HurtMediumMap.ofSeq mediums)) }
    static member createWithMediumMap (owner: string) (ownAABB: AABB) (attackAABB: AABB) (mediums: HurtMediumMap) =
      { owner = owner
        ignores = []
        ownAABB = ownAABB
        attackIntent = Some (AttackIntent.create attackAABB mediums) }

// participants register themselves with their hurtable bodies and hurting intents
// the hurtsystem calculates what participant suffers form which hurtintent

type HurtResult =
  { attackList: Map<string, HurtIntent list>
    hurtList: Map<string, HurtIntent list> }
  static member empty = { attackList = Map.empty; hurtList = Map.empty }

module HurtResult =

  let add (attacker, reciever, intent) (hr: HurtResult) =
    { attackList = hr.attackList |> Map.addToList attacker intent;
      hurtList = hr.hurtList |> Map.addToList reciever intent }


module HurtSystem =
    let calculateHurtResults (hurtSystem: HurtIntent array) =
        seq {
          for i in 0..hurtSystem.Length-1 do
            for j in i+1..hurtSystem.Length-1 do
              let intent1 = hurtSystem.[i]
              let intent2 = hurtSystem.[j]
              match intent1.attackIntent with
              | Some intent1Attack when intent1.ignores |> List.contains intent2.owner |> not ->
                if intent1Attack.rectangle.Intersects(intent2.ownAABB.rectangle) && not (intent1.ignores |> List.contains intent2.owner) then
                  yield intent1.owner, intent2.owner, intent1
              | _ -> ()
              match intent2.attackIntent with
              | Some intent2Attack when intent2.ignores |> List.contains intent1.owner |> not ->
                if intent2Attack.rectangle.Intersects(intent1.ownAABB.rectangle) && not (intent2.ignores |> List.contains intent1.owner) then
                  yield intent2.owner, intent1.owner, intent2
              | _ -> ()
        }
        |> Seq.fold (fun hr intentResult ->
            hr |> HurtResult.add intentResult
        ) HurtResult.empty
