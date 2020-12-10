// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

// Define a function to construct a message to print

let readInput (filePath:String) = seq {
    use sr = StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Var (i:int) as self =
    let sot = i
    member this.sotPlus2 () = sot
    new (s:String) = Var(2)
        
let from whom =
    sprintf "from %s" whom

let getDiffs (input: int[]) : int[] =
    { 0 .. input.Length-2 }
    |> Seq.map (fun i -> input.[i+1] - input.[i]) |> Seq.toArray
    
type Memo = Map<int,int64>

let rec combos (memo:Memo) (values: int[]) (curr:int): Memo*int64 =
    if memo.ContainsKey curr
        then (memo,memo.[curr])
    else 
        // let new_trace = Seq.append trace [values.[curr]] |> Seq.toArray
        let last = values.Length-1
        let available : int = if last - curr > 3 then 3 else last - curr
        let jump i =
            // printfn "jump [%d] = %d - %d " i values.[i] values.[curr]
            values.[i] - values.[curr]
        let is_cand i = jump i < 4;
        let num_to_try = {curr+1 .. curr+available} |> Seq.filter is_cand |> Seq.length
        // printfn "%A %d" values.[curr..curr+available] num_to_try
        // printfn "combos (values) %d available=%d to_try=%d" curr available num_to_try 
        if num_to_try > 0 then
            let recur (memo:Memo) (i:int) :Memo*int64 = combos memo values (curr + i)
            let init = (memo,0L)
            let accumulate (memo:Memo,result:int64) (i:int) : Memo*int64 =
                let (newMemo,newResult) = combos memo values (curr + i)
                (newMemo,result + newResult)
            let (resultMemo,result) = { 1.. num_to_try } |> Seq.fold accumulate init
            let newMemo = resultMemo.Add (curr, result) 
            newMemo,result 
        else if curr = last then
            // printfn "at the end: trace=%A" new_trace;
            memo,1L
        else 
            // printfn "nothing available: curr=%d" curr ;
            memo,0L
    
[<EntryPoint>]
let main argv =
    let file = "/Users/xeno/projects/aoc2020/day10_fs/input.txt"
    let input : int[] = readInput file |> Seq.map int |> Seq.sort |> Seq.toArray
    let builtinAdapter = input.[input.Length-1]+3
    let values : int[] = Seq.append input [builtinAdapter] |> Seq.toArray |> Seq.append [0] |> Seq.toArray 
    let diffs =  getDiffs values 
    let ones =  diffs |> Seq.filter (fun i -> i = 1) |> Seq.length
    let threes =  diffs |> Seq.filter (fun i -> i = 3) |> Seq.length
    printfn "Sorter: %A" values 
    printfn "Diffs: %A" diffs
    printfn "Answer 1: %d * %d = %d" ones threes (ones * threes)
    let (memo,coms) = combos Map.empty values 0
    printfn "combos: %d" coms
    0 // return an integer exit code