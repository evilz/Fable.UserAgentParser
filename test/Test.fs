module Test

open Fable.Core
open Fable.Core.JsInterop
open Fable.UserAgentParser
open Test.BowserTest

type IAssert =
    [<Emit("$0.true($1)")>]  abstract isTrue: bool -> unit
    [<Emit("$0.false($1)")>] abstract isFalse: bool -> unit
    [<Emit("$0.is($1, $2)")>] abstract is: obj -> obj -> unit
    [<Emit("$0.deepEqual($1, $2)")>] abstract deepEqual: obj -> obj -> unit


let test(name: string, f: IAssert->unit): unit = importDefault "ava"

test("The test works", fun t ->

    let browser = "Mozilla/5.0 (iPad; CPU OS 7_0_6 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B651 Safari/9537.53"
                    |> Browser.parse

    browser.name |> t.is "Mobile Safari"
)




browsers |> List.iteri ( fun i b ->
    //test(x.title, fun t -> 
        //x.list |> List.iter (fun b -> 
            //test(sprintf "%s - %s" x.title b.desc, fun t-> 
            test(sprintf "%s %A %i" b.desc b.expect.name i  , fun t-> 
                b.ua 
                |> Browser.parse
                |> t.deepEqual b.expect
                )
            )
    //) 


CpuList |> List.iteri ( fun i c ->
            test(sprintf "%s %A %i" c.desc c.expect.architecture i  , fun t-> 
                c.ua 
                |> Cpu.parse
                |> t.deepEqual c.expect
                )
            )

