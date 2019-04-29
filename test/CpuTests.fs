
namespace Test
open System


module BowserTest = 

    open Fable.UserAgentParser


    type CpuTest = 
           {
            desc: string
            ua : string
            expect  : Cpu
        }

    let CpuList = 

        [
            {
                desc    = "i686"
                ua      = "Mozilla/5.0 (X11; Ubuntu; Linux i686; rv:19.0) Gecko/20100101 Firefox/19.0"
                expect  =
                {
                    architecture  = Some "ia32"
                }
            }
            {
                desc    = "i386"
                ua      = "Mozilla/5.0 (X11; U; FreeBSD i386; en-US; rv:1.7) Gecko/20040628 Epiphany/1.2.6"
                expect  =
                {
                    architecture  = Some "ia32"
                }
            }
            {
                desc    = "x86-64"
                ua      = "Opera/9.80 (X11; Linux x86_64; U; Linux Mint; en) Presto/2.2.15 Version/10.10"
                expect  =
                {
                    architecture  = Some "amd64"
                }
            }
            {
                desc    = "win64"
                ua      = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.2; Win64; x64; Trident/6.0; .NET4.0E; .NET4.0C)"
                expect  =
                {
                    architecture  = Some "amd64"
                }
            }
            {
                desc    = "WOW64"
                ua      = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)"
                expect  =
                {
                    architecture  = Some "amd64"
                }
            }
            {
                desc    = "ARMv6"
                ua      = "Mozilla/5.0 (X11; U; Linux armv61; en-US; rv:1.9.1b2pre) Gecko/20081015 Fennec/1.0a1"
                expect  =
                {
                    architecture  = Some "arm"
                }
            }
            {
                desc    = "ARMv7"
                ua      = "Mozilla/5.0 (X11; CrOS armv7l 9765.85.0) AppleWebKit/537.36 (KHTML like Gecko) Chrome/61.0.3163.123 Safari/537.36"
                expect  =
                {
                    architecture  = Some "arm"
                }
            }
            {
                desc    = "Pocket PC"
                ua      = "Opera/9.7 (Windows Mobile; PPC; Opera Mobi/35166; U; en) Presto/2.2.1"
                expect  =
                {
                    architecture  = Some "arm"
                }
            }
            {
                desc    = "Mac PowerPC"
                ua      = "Mozilla/4.0 (compatible; MSIE 4.5; Mac_PowerPC)"
                expect  =
                {
                    architecture  = Some "ppc"
                }
            }
            {
                desc    = "Mac PowerPC"
                ua      = "Mozilla/4.0 (compatible; MSIE 5.17; Mac_PowerPC Mac OS; en)"
                expect  =
                {
                    architecture  = Some "ppc"
                }
            }
            {
                desc    = "Mac PowerPC"
                ua      = "iCab/2.9.5 (Macintosh; U; PPC; Mac OS X)"
                expect  =
                {
                    architecture  = Some "ppc"
                }
            }
            {
                desc    = "UltraSPARC"
                ua      = "Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.9b5) Gecko/2008032620 Firefox/3.0b5"
                expect  =
                {
                    architecture  = Some "sparc"
                }
            }
            {
                desc    = "QuickTime"
                ua      = "QuickTime/7.5.6 (qtver=7.5.6;cpu=IA32;os=Mac 10.5.8)"
                expect  = 
                {
                    architecture  = Some "ia32"
                }
            }
            {
                desc    = "XBMC"
                ua      = "XBMC/12.0 Git:20130127-fb595f2 (Windows NT 6.1;WOW64;Win64;x64; http://www.xbmc.org)"
                expect  = 
                {
                    architecture  = Some "amd64"
                }
            }
        ]