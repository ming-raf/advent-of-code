namespace AdventOfCode.Days.Day5

    module ModelUtility =
        let digitCount number = (int) (log10 ((float)number)) + 1;

        let replaceValue (input: list<int>) (replIndex: int) (value: int) =
            let rec _replaceValue (list: list<int>) (acc: list<int>) (value: int) (replIndex: int) (currIndex: int) : list<int> =
                match list with
                | [] -> []
                | head::tail when currIndex = replIndex -> acc @ [value] @ tail;
                | head::tail -> _replaceValue tail (acc @ [head]) value replIndex (currIndex + 1)
            _replaceValue input [] value replIndex 0

    module Model =
        open System
        open ModelUtility

        type OpCode(value: int) =

            static member Addition: OpCode = OpCode(1)
            static member Multiplication: OpCode = OpCode(2)
            static member Assign: OpCode = OpCode(3)
            static member Output: OpCode = OpCode(4)
            static member Stop: OpCode = OpCode(99)

            member this.Value =
                if (value <> 1 &&
                    value <> 2 &&
                    value <> 3 &&
                    value <> 4 &&
                    value <> 99) then
                    raise (InvalidOperationException("No match for opcode"))
                else
                    value

            override this.ToString() = string(this.Value)

            override this.Equals(code) =
                match code with
                | :? OpCode as c -> c.Value = this.Value
                | _ -> false

            override this.GetHashCode() =
                hash (this.Value)

        type ParameterMode(value: int) =
            member this.Value = value
            static member Position: ParameterMode = ParameterMode(0)
            static member Immediate: ParameterMode = ParameterMode(1)

            override this.ToString() = string(this.Value)

            override this.Equals(mode) =
                match mode with
                | :? ParameterMode as m -> m.Value = this.Value
                | _ -> false

            override this.GetHashCode() =
                hash (this.Value)

        [<AllowNullLiteral>]
        type Param(value: int) =
            member this.Value = value

            override this.ToString() = string(this.Value )

        type Input(value: int) =
            inherit Param(value)

            let reversedValues = string(value) |> Seq.toList |> List.rev |> List.map (fun c -> int c - int '0')

            let rec getInput (list: List<int>) (count: int) (acc: OpCode * List<ParameterMode>) =
                match list with
                | [] -> acc
                | head::tail when count = 0 -> let doubleOpCode = let secondDigit = if not tail.IsEmpty then tail.[0] else 0
                                                                  (secondDigit * 10) + head
                                               getInput tail (count + 1) (OpCode(doubleOpCode), [])
                | head::tail when count = 1 -> getInput tail (count + 1) acc
                | head::tail -> let (op, ps) = acc
                                getInput tail (count + 1) (op, ps @ [ParameterMode(head)])

            let (opcode, modes) = getInput reversedValues 0 (OpCode.Addition, [])

            member this.OpCode: OpCode = opcode
            member this.Modes: List<ParameterMode> = modes

            override this.ToString() = string(this.Value)

        type Instruction =
            val Input: Input
            val P1: Param
            val P2: Param
            val Output: Param

            new (a: Input) =
                { Input = a; P1 = null; P2 = null; Output = null; }

            new (a: Input, b: Param) =
                { Input = a; P1 = b; P2 = null; Output = null; }

            new (a: Input, b: Param, c: Param, d: Param) =
                { Input = a; P1 = b; P2 = c; Output = d; }

            override this.ToString() =
                // if this.Input.ToString() // + " " + this.P1.ToString() + " " + this.P2.ToString() + " " + this.Output.ToString()
                let printInput = this.Input.ToString()
                let printP1 = if not (isNull this.P1) then this.P1.ToString() else "null"
                let printP2 = if not (isNull this.P2) then this.P2.ToString() else "null"
                let printOutput = if not (isNull this.Output) then this.Output.ToString() else "null"

                printInput + " " + printP1 + " " + printP2 + " " + printOutput

            member this.GetInstructionSize =
                let count = 1
                let count = if not (isNull this.P1) then count + 1 else count
                let count = if not (isNull this.P2) then count + 1 else count
                let count = if not (isNull this.Output) then count + 1 else count
                count

            member private this.GetAddOrMultValues (input: List<int>) =
                let size = this.GetInstructionSize
                // printfn "Instruction Size: %d" size
                // printfn "Input: %A" input
                // printfn "Instruction: %A" this
                let modes = if this.Input.Modes.Length < size then
                                let diff = size - this.Input.Modes.Length - 1
                                let leftoverModes = Array.create diff 0 |> Array.toList |> List.map ParameterMode
                                this.Input.Modes @ leftoverModes
                            else this.Input.Modes
                let p1Value = if modes.[0] = ParameterMode.Position then
                                input.[this.P1.Value]
                              else
                                this.P1.Value
                let p2Value = if modes.[1] = ParameterMode.Position then
                                input.[this.P2.Value]
                              else
                                this.P2.Value
                // No difference for now
                let outputValue = if modes.[2] = ParameterMode.Position then
                                    this.Output.Value
                                  else
                                    this.Output.Value
                // printfn "%A %d %d %d" modes p1Value p2Value outputValue
                (p1Value, p2Value, outputValue)

            member private this.EvaluateAddition (input: List<int>): List<Param> =
                let (p1Value, p2Value, outputValue) = this.GetAddOrMultValues input
                let result = replaceValue input outputValue (p1Value + p2Value)
                // printfn "Replaced %A" result
                result |> List.map (Param)

            member private this.EvaluateMultiplication (input: List<int>): List<Param> =
                let (p1Value, p2Value, outputValue) = this.GetAddOrMultValues input
                let result = replaceValue input outputValue (p1Value * p2Value)
                // printfn "Replaced %A" result
                result |> List.map (Param)

            member private this.EvaluateAssignation (input: List<int>): List<Param> =
                let size = this.GetInstructionSize
                // printfn "Instruction Size: %d" size
                // printfn "Input: %A" input
                // printfn "Instruction: %A" this
                let modes = if this.Input.Modes.Length < size then
                                let diff = size - this.Input.Modes.Length - 1
                                let leftoverModes = Array.create diff 0 |> Array.toList |> List.map ParameterMode
                                this.Input.Modes @ leftoverModes
                            else this.Input.Modes
                let userInput = printf "User Input: "; int(Console.ReadLine())
                let outputValue = this.P1.Value
                // printfn "%A %d" modes outputValue
                let result = replaceValue input outputValue userInput
                // printfn "Replaced %A" result
                result |> List.map (Param)

            member private this.EvaluateOutput (input: List<int>): int =
                let size = this.GetInstructionSize
                // printfn "Instruction Size: %d" size
                // printfn "Input: %A" input
                // printfn "Instruction: %A" this
                let modes = if this.Input.Modes.Length < size then
                                let diff = size - this.Input.Modes.Length - 1
                                let leftoverModes = Array.create diff 0 |> Array.toList |> List.map ParameterMode
                                this.Input.Modes @ leftoverModes
                            else this.Input.Modes
                let output = if modes.[0] = ParameterMode.Position then
                                input.[this.P1.Value]
                              else
                                this.P1.Value
                // printfn "%A" modes
                printfn "OUTPUT: %d" output
                output

            member this.Evaluate (input: List<Param>): List<Param> =
                let inputActual = input |> List.map (fun x -> x.Value)
                // printfn "Input Actual: %A, OpCode: %A" inputActual this.Input.OpCode

                if this.Input.OpCode = OpCode.Addition then
                    this.EvaluateAddition inputActual
                elif this.Input.OpCode = OpCode.Multiplication then
                    this.EvaluateMultiplication inputActual
                elif this.Input.OpCode = OpCode.Assign then
                    this.EvaluateAssignation inputActual
                elif this.Input.OpCode = OpCode.Output then
                    let output = this.EvaluateOutput inputActual
                    input
                else input


