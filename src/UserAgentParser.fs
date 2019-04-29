namespace Fable

open System
open System.Text.RegularExpressions
open System.Runtime.InteropServices

module UserAgentParser =


    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    
    type Browser = {name: string option ; version: string option}


    module Browser =

        let empty = { Browser.name = None; version = None}

        let withName name browser = {browser with name = Some name} 
        let withVersion version browser = {browser with version = Some version}

        let withNameRgx (pattern:string) (replacement:string) browser=
            browser
            |> withName (Regex.Replace(browser.name.Value, pattern, replacement))  // TODO : if name is none can fail !!!!
            
        let (|BrowserNameVersion|_|) pattern input =
            let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
            if m.Success then 
                let values = List.tail [ for g in m.Groups -> g.Value ] // skip first one
                match values with
                | name::version::_ ->
                    if String.IsNullOrEmpty(version) then Some {empty with name = Some name ; version= None }
                    else Some {empty with name = Some name ; version= Some version }
                | name::_ -> Some {empty with name = Some name ; version= None }
                | _ -> Some empty
            else None

        let (|BrowserVersion|_|) pattern input =
            let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
            if m.Success then 
                let values = List.tail [ for g in m.Groups -> g.Value ] // skip first one
                match values with
                | version::_ -> 
                    if String.IsNullOrEmpty(version) then Some empty
                    else Some {empty with version= Some version }
                | _ -> Some empty
            else None

        let (|BrowserVersionName|_|) pattern input =
            let m = Regex.Match(input, pattern, RegexOptions.IgnoreCase)
            if m.Success then 
                let values = List.tail [ for g in m.Groups -> g.Value ] // skip first one
                match values with
                | version::name::_ ->
                    if String.IsNullOrEmpty(version) then Some {empty with name = Some name ; version= None }
                    else Some {empty with name = Some name ; version= Some version }
                | name::_ -> Some {empty with name = Some name ; version= None }
                | _ -> Some empty
            else None

       
        let (|OldSafariVersion|) browser =
            match browser.version with
            | None -> browser
            |  Some v when v.StartsWith "/8" -> browser |> withVersion "1.0"
            |  Some v when v.StartsWith "/1" -> browser |> withVersion "1.2"
            |  Some v when v.StartsWith "/3" -> browser |> withVersion "1.3"
            |  Some v when v.StartsWith "/412" -> browser |> withVersion "2.0"
            |  Some v when v.StartsWith "/416" -> browser |> withVersion "2.0.2"
            |  Some v when v.StartsWith "/417" -> browser |> withVersion "2.0.3"
            |  Some v when v.StartsWith "/419" -> browser |> withVersion "2.0.4"
            | _ -> browser


        let parse input = 
            match input with
            
            // Presto based
            | BrowserNameVersion "(opera\\smini)\\/([\\w\\.-]+)" b -> b                                           // Opera mini
            | BrowserNameVersion "(opera\\s[mobiletab]+).+version\\/([\\w\\.-]+)" b -> b                          // Opera Mobi/Tablet
            | BrowserNameVersion "(opera).+version\\/([\\w\\.]+)" b -> b                                          // Opera > 9.80
            | BrowserNameVersion "(opera)[\\/\\s]+([\\w\\.]+)"  b -> b                                            // Opera < 9.80
            
            | BrowserNameVersion "(opios)[\/\s]+([\w\.]+)" b -> b |> withName "Opera Mini"                        // Opera mini on iphone >= 8.0
            
            | BrowserNameVersion "\s(opr)\/([\w\.]+)" b -> b |> withName"Opera"                                   // Opera Webkit
         
            // Mixed
            | BrowserNameVersion "(kindle)\/([\w\.]+)"  b -> b                                                    // Kindle
            | BrowserNameVersion "(lunascape|maxthon|netfront|jasmine|blazer)[\/\s]?([\w\.]*)" b -> b             // Lunascape/Maxthon/Netfront/Jasmine/Blazer

            // Trident based
            | BrowserNameVersion "(avant\s|iemobile|slim|baidu)(?:browser)?[\/\s]?([\w\.]*)" b-> b                // Avant/IEMobile/SlimBrowser/Baidu
            | BrowserNameVersion "(?:ms|\()(ie)\s([\w\.]+)" b -> b                                                 // Internet Explorer

             // Webkit/KHTML based
            | BrowserNameVersion "(rekonq)\/([\w\.]*)" b -> b                                                     // Rekonq
            | BrowserNameVersion "(chromium|flock|rockmelt|midori|epiphany|silk|skyfire|ovibrowser|bolt|iron|vivaldi|iridium|phantomjs|bowser|quark|qupzilla|falkon)\/([\w\.-]+)"  b-> b  // Chromium/Flock/RockMelt/Midori/Epiphany/Silk/Skyfire/Bolt/Iron/Iridium/PhantomJS/Bowser/QupZilla/Falkon
            
            | BrowserNameVersion "(konqueror)\/([\w\.]+)" b -> b |> withName "Konqueror"                          // Konqueror
             
            | BrowserNameVersion "(trident).+rv[:\s]([\w\.]+).+like\sgecko" b -> b |> withName "IE"               // IE11
            
            | BrowserNameVersion "(edge|edgios|edga)\/((\d+)?[\w\.]+)" b -> b |> withName "Edge"                  // Microsoft Edge
             
            | BrowserNameVersion "(yabrowser)\/([\w\.]+)" b -> b |> withName "Yandex"                             // Yandex
              
            | BrowserNameVersion "(puffin)\/([\w\.]+)" b -> b |> withName "Puffin"                                // Puffin
             
            | BrowserNameVersion "(focus)\/([\w\.]+)" b -> b |> withName "Firefox Focus"                          // Firefox Focus
            
            | BrowserNameVersion "(opt)\/([\w\.]+)" b -> b |> withName "Opera Touch"                              // Opera Touch
             
            | BrowserNameVersion "((?:[\s\/])uc?\s?browser|(?:juc.+)ucweb)[\/\s]?([\w\.]+)" b -> b |> withName "UCBrowser"    // UCBrowser
           
            | BrowserNameVersion "(comodo_dragon)\/([\w\.]+)" b -> b |> withName "Comodo Dragon"  // /_/g, ' '    // Comodo Dragon
              
            | BrowserNameVersion "(micromessenger)\/([\w\.]+)" b -> b |> withName "WeChat"                        // WeChat
             
            | BrowserNameVersion "(brave)\/([\w\.]+)" b -> b |> withName "Brave"                                  // Brave browser
             
            | BrowserNameVersion "(qqbrowserlite)\/([\w\.]+)" b -> b                                              // QQBrowserLite
             
            | BrowserNameVersion "(QQ)\/([\d\.]+)" b -> b                                                         // QQ, aka ShouQ
             
            | BrowserNameVersion "m?(qqbrowser)[\/\s]?([\w\.]+)" b -> b                                           // QQBrowser
            
            | BrowserNameVersion "(BIDUBrowser)[\/\s]?([\w\.]+)" b -> b                                           // Baidu Browser
              
            | BrowserNameVersion "(2345Explorer)[\/\s]?([\w\.]+)" b -> b                                          // 2345 Browser
               
            | BrowserNameVersion "(MetaSr)[\/\s]?([\w\.]+)" b -> b                                               // SouGouBrowser

            | BrowserNameVersion "(LBBROWSER)" b -> b                                                            // LieBao Browser

            | BrowserVersion "xiaomi\/miuibrowser\/([\w\.]+)" b -> b |> withName  "MIUI Browser"                            // MIUI Browser
             
            | BrowserVersion ";fbav\/([\w\.]+);" b -> b|> withName "Facebook"                                           // Facebook App for iOS & Android
             
             
            | BrowserNameVersion "safari\s(line)\/([\w\.]+)" b -> b                                      // Line App for iOS
            | BrowserNameVersion "android.+(line)\/([\w\.]+)\/iab" b -> b                                  // Line App for Android
              
            | BrowserVersion "headlesschrome(?:\/([\w\.]+)|\s)" b -> b |> withName  "Chrome Headless"   // Chrome Headless
             
            | BrowserNameVersion "\swv\).+(chrome)\/([\w\.]+)" b -> b  |> withNameRgx  "(.+)"  "$1 WebView" // Chrome WebView
            
            | BrowserNameVersion "((?:oculus|samsung)browser)\/([\w\.]+)" b -> b  |> withNameRgx  "(.+(?:g|us))(.+)"  "$1 $2" // Oculus / Samsung Browser

            | BrowserVersion "android.+version\/([\w\.]+)\s+(?:mobile\s?safari|safari)*" b -> b |> withName  "Android Browser"        // Android Browser
            
            | BrowserNameVersion "(chrome|omniweb|arora|[tizenoka]{5}\s?browser)\/v?([\w\.]+)" b -> b // Chrome/OmniWeb/Arora/Tizen/Nokia
              
            | BrowserNameVersion "(dolfin)\/([\w\.]+)" b -> b |> withName  "Dolphin"                                              // Dolphin
             
             
            | BrowserNameVersion "((?:android.+)crmo|crios)\/([\w\.]+)" b -> b |> withName "Chrome"                            // Chrome for Android/iOS
            
            | BrowserNameVersion "(coast)\/([\w\.]+)" b -> b |> withName "Opera Coast"                                               // Opera Coast
              
            | BrowserVersion "fxios\/([\w\.-]+)" b -> b |> withName "Firefox"                                               // Firefox for iOS
              
            | BrowserVersion "version\/([\w\.]+).+?mobile\/\w+\s(safari)" b -> b |> withName "Mobile Safari"                      // Mobile Safari
             
            | BrowserVersionName "version\/([\w\.]+).+?(mobile\s?safari|safari)" b -> b                  // Safari & Safari Mobile
              
            | BrowserNameVersion "webkit.+?(gsa)\/([\w\.]+).+?(mobile\s?safari|safari)(\/[\w\.]+)" b -> b |> withName "GSA"  // Google Search Appliance on iOS
             
            | BrowserNameVersion "webkit.+?(mobile\s?safari|safari)(\/[\w\.]+)" b -> match b with OldSafariVersion x -> x

            | BrowserNameVersion "(webkit|khtml)\/([\w\.]+)" b -> b

                // Gecko based
            | BrowserNameVersion "(navigator|netscape)\/([\w\.-]+)" b -> b |> withName "Netscape"                               // Netscape
             
             
            | BrowserNameVersion "(swiftfox)" b -> b                                                     // Swiftfox
            | BrowserNameVersion "(icedragon|iceweasel|camino|chimera|fennec|maemo\sbrowser|minimo|conkeror)[\/\s]?([\w\.\+]+)" b -> b // IceDragon/Iceweasel/Camino/Chimera/Fennec/Maemo/Minimo/Conkeror
            | BrowserNameVersion "(firefox|seamonkey|k-meleon|icecat|iceape|firebird|phoenix|palemoon|basilisk|waterfox)\/([\w\.-]+)$" b -> b

                                                                                    // Firefox/SeaMonkey/K-Meleon/IceCat/IceApe/Firebird/Phoenix
            | BrowserNameVersion "(mozilla)\/([\w\.]+).+rv\:.+gecko\/\d+" b -> b                         // Mozilla

                // Other
            | BrowserNameVersion "(polaris|lynx|dillo|icab|doris|amaya|w3m|netsurf|sleipnir)[\/\s]?([\w\.]+)" b -> b
                                                                                    // Polaris/Lynx/Dillo/iCab/Doris/Amaya/w3m/NetSurf/Sleipnir
            | BrowserNameVersion "(links)\s\(([\w\.]+)" b -> b                                            // Links
            | BrowserNameVersion "(gobrowser)\/?([\w\.]*)" b -> b                                         // GoBrowser
            | BrowserNameVersion "(ice\s?browser)\/v?([\w\._]+)" b -> b                                 // ICE Browser
            | BrowserNameVersion "(mosaic)[\/\s]([\w\.]+)" b -> b                                        // Mosaic
            | _ -> {name = None; version = None}

    type Cpu = { architecture: string option}

    module Cpu =

        let parse input = 
            match input with
            | Regex "(?:(amd|x(?:(?:86|64)[_-])?|wow|win)64)[;\)]" x -> {architecture = Some "amd64"}                    // AMD64
            | Regex "(ia32(?=;))" x -> {architecture =  x.Head |> fun s -> s.ToLowerInvariant() |> Some }                    // IA32 (quicktime)
            | Regex "((?:i[346]|x)86)[;\)]" x -> {architecture = Some "ia32"}                // IA32
            | Regex "windows\s(ce|mobile);\sppc" x -> {architecture = Some "arm"}     // PocketPC mistakenly identified as PowerPC
            
            // [[ARCHITECTURE, /ower/, '', util.lowerize]],
            | Regex "((?:ppc|powerpc)(?:64)?)(?:\smac|;|\))" x -> {architecture = Some "ppc" }  // PowerPC
            | Regex "(sun4\w)[;\)]" x -> {architecture = Some "sparc"} // SPARC
            | Regex "((?:avr32|ia64(?=;))|68k(?=\))|arm(?:64|(?=v\d+[;l]))|(?=atmel\s)avr|(?:irix|mips|sparc)(?:64)?(?=;)|pa-risc)" x -> {architecture = x.Head |> fun s -> s.ToLowerInvariant() |> Some } // IA64, 68K, ARM/64, AVR/32, IRIX/64, MIPS/64, SPARC/64, PA-RISC
            | _ -> {architecture = None}


    type Device = { vendor: string option; ``type``: string option; model: string option }

    type OS = { version: string option; name: string option }

    type UserAgentData = {
        agent: string
        browser: Browser
        //engine: Engine
        //os: OS
        //device: Device
        cpu: Cpu
    }

    let parse input =
        let browser = Browser.parse input
        let cpu = Cpu.parse input

        {
            agent = input
            browser = browser
            cpu = cpu
        }
