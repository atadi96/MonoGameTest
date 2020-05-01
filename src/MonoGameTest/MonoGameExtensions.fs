[<AutoOpen>]
module MonoGameExtensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module Map =
    let addMonoid (zero: 'u) (modify: 'u -> 'v -> 'u) (k: 'k) (v: 'v) (map: Map<'k,'u>) =
        match map |> Map.tryFind k with
        | None -> map |> Map.add k (modify zero v)
        | Some vs -> map |> Map.add k (modify vs v)
    let addToList (k: 'k) (v: 'v) (map: Map<'k,'v list>) = addMonoid [] (fun xs x -> x :: xs) k v map

type Viewport with
    member this.Center = Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)

type Vector2 with
    member this.MajorAxis() =
        if abs this.X > abs this.Y then
            Vector2(float32 (sign this.X), 0.f)
        else
            Vector2(0.f, float32 (sign this.Y))
            
    static member NormalizeOrZero(vec: Vector2) =
        if vec = Vector2.Zero then
            vec
        else
            Vector2.Normalize vec

let (|KeyDown|_|) k (state: KeyboardState) =
    if state.IsKeyDown k then Some () else None


let (|KeyUp|_|) k (state: KeyboardState) =
    if state.IsKeyUp k then None else Some ()

let (|KeyPress|_|) k (prevState: KeyboardState, state: KeyboardState) =
    if prevState.IsKeyUp k && state.IsKeyDown k then Some () else None

module InputState =
    type InputState =
        { prevMouse: MouseState
          currMouse: MouseState
          prevKeyb: KeyboardState
          currKeyb: KeyboardState }
        member this.MousePosition = this.currMouse.Position

    module InputState =
        let update (is: InputState) =
            { is with
                prevMouse = is.currMouse
                prevKeyb = is.currKeyb
                currMouse = Mouse.GetState()
                currKeyb = Keyboard.GetState() }

        let init() =
            { prevMouse = Unchecked.defaultof<MouseState>
              prevKeyb = Unchecked.defaultof<KeyboardState>
              currMouse = Mouse.GetState()
              currKeyb = Keyboard.GetState() }

    let (|KeyDown|_|) k (state: InputState) =
        if state.currKeyb.IsKeyDown k then Some () else None


    let (|KeyUp|_|) k (state: InputState) =
        if state.currKeyb.IsKeyUp k then Some () else None

    let (|KeyPress|_|) k (state: InputState) =
        if state.prevKeyb.IsKeyUp k && state.currKeyb.IsKeyDown k then Some () else None

    let (|KeyRelease|_|) k (state: InputState) =
        if state.prevKeyb.IsKeyDown k && state.currKeyb.IsKeyUp k then Some () else None

    type MouseButton =
        | LeftMouseButton
        | MiddleMouseButton
        | RightMouseButton

    let private mouseButton b (ms: MouseState) =
        match b with
        | LeftMouseButton -> ms.LeftButton
        | MiddleMouseButton -> ms.MiddleButton
        | RightMouseButton -> ms.RightButton

    let (|MouseDown|_|) (b: MouseButton) (state: InputState) =
        if mouseButton b state.currMouse = ButtonState.Pressed then Some () else None

    let (|MouseUp|_|) (b: MouseButton) (state: InputState) =
        if mouseButton b state.currMouse = ButtonState.Released then Some () else None

    let (|MouseClick|_|) (b: MouseButton) (state: InputState) =
        match mouseButton b state.prevMouse, mouseButton b state.currMouse with
        | ButtonState.Released, ButtonState.Pressed -> Some ()
        | _ -> None

    let (|MouseRelease|_|) (b: MouseButton) (state: InputState) =
        match mouseButton b state.prevMouse, mouseButton b state.currMouse with
        | ButtonState.Pressed, ButtonState.Released -> Some ()
        | _ -> None