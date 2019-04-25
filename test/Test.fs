module Test

open Fable.Core
open Fable.Core.JsInterop
open Fable.UserAgentParser

type IAssert =
    [<Emit("$0.true($1)")>]  abstract isTrue: bool -> unit
    [<Emit("$0.false($1)")>] abstract isFalse: bool -> unit
    [<Emit("$0.is($1, $2)")>] abstract is: obj -> obj -> unit

let test(name: string, f: IAssert->unit): unit = importDefault "ava"

test("The test works", fun t ->

    let browser = "Mozilla/5.0 (iPad; CPU OS 7_0_6 like Mac OS X) AppleWebKit/537.51.1 (KHTML, like Gecko) Version/7.0 Mobile/11B651 Safari/9537.53"
                    |> Browser.parse

    browser.name |> t.is "Mobile Safari"
)