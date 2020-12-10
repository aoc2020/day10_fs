open System
open System.IO

let readInput (filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let getDiffs (input: int[]) : int[] =
    { 0 .. input.Length-2 }
    |> Seq.map (fun i -> input.[i+1] - input.[i]) |> Seq.toArray
    
type Memo = Map<int,int64>

let rec combos (memo:Memo) (values: int[]) (curr:int): Memo*int64 =
    if memo.ContainsKey curr
        then (memo,memo.[curr])
    else 
        let last = values.Length-1
        let available : int = if last - curr > 3 then 3 else last - curr
        let jump i = values.[i] - values.[curr]
        let is_cand i = jump i < 4;
        let num_to_try = {curr+1 .. curr+available} |> Seq.filter is_cand |> Seq.length
        if num_to_try > 0 then
            let init = (memo,0L)
            let accumulate (memo:Memo,result:int64) (i:int) : Memo*int64 =
                let (newMemo,newResult) = combos memo values (curr + i)
                (newMemo,result + newResult)
            let (resultMemo,result) = { 1.. num_to_try } |> Seq.fold accumulate init
            let newMemo = resultMemo.Add (curr, result) 
            newMemo,result 
        else if curr = last then
            memo,1L
        else 
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
    0