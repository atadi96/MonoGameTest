module Element
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Element =
    //| Water
    | Life
    //| Shield
    //| Cold
    //| Lightning
    //| Arcane
    | Earth
    //| Fire
    //| Ice
    //| Poison
    //| Steam

module Element =
    let removes = function
        //| Water -> [Fire; Cold; Lightning]
        | Life -> [] //[Arcane]
        //| Shield -> [Shield]
        //| Cold -> [Fire; Water]
        //| Lightning -> [Earth; Water]
        //| Arcane -> [Life]
        | Earth -> []// [Lightning]
        //| Fire -> [Cold; Water]
        //| _ -> failwith "NotImplemented"

    let color = function
        //| Water -> Color.Blue
        | Life -> Color.Green
        //| Shield -> Color.Yellow
        //| Cold -> Color.LightCyan
        //| Lightning -> Color.Purple
        //| Arcane -> Color.Red
        | Earth -> Color.Brown
        //| Fire -> Color.Orange
        //| _ -> failwith "NotImplemented"

    let fromKeyboard (prevState: KeyboardState) (state: KeyboardState) =
        match prevState, state with
        //| KeyPress Keys.Q -> Some Water
        | KeyPress Keys.W -> Some Life
        //| KeyPress Keys.E -> Some Shield
        //| KeyPress Keys.R -> Some Cold
        //| KeyPress Keys.A -> Some Lightning
        //| KeyPress Keys.S -> Some Arcane
        | KeyPress Keys.D -> Some Earth
        //| KeyPress Keys.F -> Some Fire
        | _ -> None

    let rec private rewind xs ys =
        match xs with
        | [] -> ys
        | x :: xs -> rewind xs (x :: ys)
    let conjure (elements: Element list) (element: Element) =
        let rec conjure (pastElements: Element list) (elements: Element list) (element: Element) =
            match elements with
            | [] ->
                let newElement =
                    if pastElements.Length < 5 then
                        [element]
                    else
                        []
                rewind pastElements newElement
            | e :: es when List.contains e (removes element) -> rewind pastElements es
            | e :: es -> conjure (e :: pastElements) es element
        conjure [] elements element

    let draw (topCenter: Vector2) (elements: Element list) (spriteBatch: SpriteBatch) =
        let radius = 5.f
        let padding = 1.f
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
        let allWidth = float32 elements.Length * (2.f * radius + 2.f * padding) - 2.f * padding
        let topLeft = topCenter + Vector2(- allWidth / 2.f, 14.f)
        let circleTexture = circle 100
        for (i,element) in elements |> Seq.indexed do
            let currentTopLeft = topLeft + Vector2(float32 i * 2.f * (radius + padding), 0.f)
            let rect = Rectangle(int currentTopLeft.X, int currentTopLeft.Y, int (2.f * radius), int (2.f * radius))
                
            spriteBatch.Draw(circleTexture, rect, color element)
        //circleTexture.Dispose()

type SpellType =
    | ProjectileSpell of ProjectileSpell
    | LifeSpell
and ProjectileSpell =
    | EarthProjectile

module SpellType =
    let ofElement e =
        match e with
        | Life -> LifeSpell
        | Earth -> ProjectileSpell EarthProjectile

type ElementCollection =
    private | EC of Element list
    member this.IsEmpty =
        match this with
        | EC es -> es.IsEmpty
    member this.Elements =
        match this with
        | EC es -> es
    member this.Length = this.Elements.Length

module ElementCollection =
    let empty = EC []
    let conjure e (EC es) = EC (Element.conjure es e)
    let elements (es: ElementCollection) = es.Elements
    let spell (EC es) =
        if es |> List.isEmpty then None else
        es
        |> List.countBy id
        |> Seq.map (fun (e,n) -> SpellType.ofElement e)
        |> Seq.min
        |> Some
