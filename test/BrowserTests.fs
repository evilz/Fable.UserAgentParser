namespace Test
open System


module BowserTest = 

    open Fable.UserAgentParser


    type BrowserTest = 
           {
            desc: string
            ua : string
            expect  : Browser
            
        }

    let browsers = 
        [
            {
                desc = "Android Browser on Galaxy Nexus"
                ua  = "Mozilla/5.0 (Linux; U; Android 4.0.2; en-us; Galaxy Nexus Build/ICL53F) AppleWebKit/534.30 (KHTML like Gecko) Version/4.0 Mobile Safari/534.30"
                expect  =
                   {
                    name = Some"Android Browser"
                    version = Some "4.0"
                    //"major"   : "4"
                }
            }
            {
                desc = "Android Browser on Galaxy S3"
                ua  = "Mozilla/5.0 (Linux; Android 4.4.4; en-us; SAMSUNG GT-I9300I Build/KTU84P) AppleWebKit/537.36 (KHTML like Gecko) Version/1.5 Chrome/28.0.1500.94 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"Android Browser"
                    version = Some "1.5"
                    //"major"   : "1"
                }
            }
            {
                desc = "Android Browser on HTC Flyer (P510E)"
                ua  = "Mozilla/5.0 (Linux; U; Android 3.2.1; ru-ru; HTC Flyer P510e Build/HTK75C) AppleWebKit/534.13 (KHTML like Gecko) Version/4.0 Safari/534.13"
                expect  =
                   {
                    name = Some"Android Browser"
                    version = Some "4.0"
                    //"major"   : "4"
                }
            }
            {
                desc = "Android Browser on Huawei Honor Glory II (U9508)"
                ua  = "Mozilla/5.0 (Linux; U; Android 4.0.4; ru-by; HUAWEI U9508 Build/HuaweiU9508) AppleWebKit/534.30 (KHTML like Gecko) Version/4.0 Mobile Safari/534.30 ACHEETAHI/2100050044"
                expect  =
                   {
                    name = Some"Android Browser"
                    version = Some "4.0"
                    //"major"   : "4"
                }
            }
            {
                desc = "Android Browser on Huawei P8 (H891L)"
                ua  = "Mozilla/5.0 (Linux; Android 4.4.4; HUAWEI H891L Build/HuaweiH891L) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/33.0.0.0 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"Android Browser"
                    version = Some "4.0"
                    //"major"   : "4"
                }
            }
            {
                desc = "Android Browser on Samsung S6 (SM-G925F)"
                ua  = "Mozilla/5.0 (Linux; Android 5.0.2; SAMSUNG SM-G925F Build/LRX22G) AppleWebKit/537.36 (KHTML like Gecko) SamsungBrowser/3.0 Chrome/38.0.2125.102 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"Samsung Browser"
                    version = Some "3.0"
                    //"major"   : "3"
                }
            }
            {
                desc = "Arora"
                ua  = "Mozilla/5.0 (Windows; U; Windows NT 5.1; de-CH) AppleWebKit/523.15 (KHTML like Gecko Safari/419.3) Arora/0.2"
                expect  =
                   {
                    name = Some"Arora"
                    version = Some "0.2"
                    //"major"   : "0"
                }
            }
            {
                desc = "Avant"
                ua  = "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; GTB5; Avant Browser; .NET CLR 1.1.4322; .NET CLR 2.0.50727)"
                expect  =
                   {
                    name = Some"Avant "
                    version = None //Some "undefined"
                    //"major"   : "undefined"
                }
            }
            {
                desc = "Baidu"
                ua  = "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; baidubrowser 1.x)"
                expect  =
                   {
                    name = Some"baidu"
                    version = Some "1.x"
                    //"major"   : "1"
                }
            }
            {
                desc = "Bolt"
                ua  = "Mozilla/5.0 (X11; 78; CentOS; US-en) AppleWebKit/527+ (KHTML like Gecko) Bolt/0.862 Version/3.0 Safari/523.15"
                expect  =
                   {
                    name = Some"Bolt"
                    version = Some "0.862"
                    //"major"   : "0"
                }
            }
            {
                desc = "Bowser"
                ua  = "Mozilla/5.0 (iOS; like Mac OS X) AppleWebKit/536.36 (KHTML like Gecko) not Chrome/27.0.1500.95 Mobile/10B141 Safari/537.36 Bowser/0.2.1"
                expect  =
                   {
                    name = Some"Bowser"
                    version = Some "0.2.1"
                    //"major"   : "0"
                }
            }
            {
                desc = "Camino"
                ua  = "Mozilla/5.0 (Macintosh; U; PPC Mac OS X 10.4; en; rv:1.9.0.19) Gecko/2011091218 Camino/2.0.9 (like Firefox/3.0.19)"
                expect  =
                   {
                    name = Some"Camino"
                    version = Some "2.0.9"
                    //"major"   : "2"
                }
            }
            {
                desc = "Chimera"
                ua  = "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; pl-PL; rv:1.0.1) Gecko/20021111 Chimera/0.6"
                expect  =
                   {
                    name = Some"Chimera"
                    version = Some "0.6"
                    //"major"   : "0"
                }
            }
            {
                desc = "Chrome"
                ua  = "Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML like Gecko) Chrome/20.0.1090.0 Safari/536.6"
                expect  =
                   {
                    name = Some"Chrome"
                    version = Some "20.0.1090.0"
                    //"major"   : "20"
                }
            }
            {
                    desc = "Chrome Headless"
                    ua  = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML like Gecko) HeadlessChrome Safari/537.36"
                    expect  =
                       {
                       name = Some"Chrome Headless"
                       version = None //Some "undefined"
                       //"major"   : "undefined"
                    }
            }
            {
                    desc = "Chrome Headless"
                    ua  = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML like Gecko) HeadlessChrome/60.0.3112.113 Safari/537.36"
                    expect  =
                       {
                       name = Some"Chrome Headless"
                       version = Some "60.0.3112.113"
                       //"major"   : "60"
                    }
            }
            {
                desc = "Chrome WebView"
                ua  = "Mozilla/5.0 (Linux; Android 5.1.1; Nexus 5 Build/LMY48B; wv) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/43.0.2357.65 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"Chrome WebView"
                    version = Some "43.0.2357.65"
                    //"major"   : "43"
                }
            }
            {
                desc = "Chrome on iOS"
                ua  = "Mozilla/5.0 (iPhone; U; CPU iPhone OS 5_1_1 like Mac OS X; en) AppleWebKit/534.46.0 (KHTML like Gecko) CriOS/19.0.1084.60 Mobile/9B206 Safari/7534.48.3"
                expect  =
                   {
                    name = Some"Chrome"
                    version = Some "19.0.1084.60"
                    //"major"   : "19"
                }
            }
            {
                desc = "Chromium"
                ua  = "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.7 (KHTML like Gecko) Ubuntu/11.10 Chromium/16.0.912.21 Chrome/16.0.912.21 Safari/535.7"
                expect  =
                   {
                    name = Some"Chromium"
                    version = Some "16.0.912.21"
                    //"major"   : "16"
                }
            }
            {
                desc = "Chrome on Android"
                ua  = "Mozilla/5.0 (Linux; U; Android-4.0.3; en-us; Galaxy Nexus Build/IML74K) AppleWebKit/535.7 (KHTML like Gecko) CrMo/16.0.912.75 Mobile Safari/535.7"
                expect  =
                   {
                    name = Some"Chrome"
                    version = Some "16.0.912.75"
                    //"major"   : "16"
                }
            }
            {
                desc = "Dillo"
                ua  = "Dillo/2.2"
                expect  =
                   {
                    name = Some"Dillo"
                    version = Some "2.2"
                    //"major"   : "2"
                }
            }
            {
                desc = "Dolphin"
                ua  = "Mozilla/5.0 (SCH-F859/F859DG12;U;NUCLEUS/2.1;Profile/MIDP-2.1 Configuration/CLDC-1.1;480*800;CTC/2.0) Dolfin/2.0"
                expect  =
                   {
                    name = Some"Dolphin"
                    version = Some "2.0"
                    //"major"   : "2"
                }
            }
            {
                desc = "Doris"
                ua  = "Doris/1.15 [en] (Symbian)"
                expect  =
                   {
                    name = Some"Doris"
                    version = Some "1.15"
                    //"major"   : "1"
                }
            }
            {
                desc = "Epiphany"
                ua  = "Mozilla/5.0 (X11; U; FreeBSD i386; en-US; rv:1.7) Gecko/20040628 Epiphany/1.2.6"
                expect  =
                   {
                    name = Some"Epiphany"
                    version = Some "1.2.6"
                    //"major"   : "1"
                }
            }
            {
                desc = "Waterfox"
                ua  = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Firefox/55.2.2 Waterfox/55.2.2"
                expect  =
                   {
                    name = Some"Waterfox"
                    version = Some "55.2.2"
                    //"major"   : "55"
                }
            }
            {
                desc = "PaleMoon"
                ua  = "Mozilla/5.0 (X11; Linux x86_64; rv:52.9) Gecko/20100101 Goanna/3.4 Firefox/52.9 PaleMoon/27.6.1"
                expect  =
                   {
                    name = Some"PaleMoon"
                    version = Some "27.6.1"
                    //"major"   : "27"
                }
            }
            {
                desc = "Basilisk"
                ua  = "Mozilla/5.0 (X11; Linux x86_64; rv:55.0) Gecko/20100101 Goanna/4.0 Firefox/55.0 Basilisk/20171113"
                expect  =
                   {
                    name = Some"Basilisk"
                    version = Some "20171113"
                    //"major"   : "20171113"
                }
            }
            {
                desc = "Facebook in-App Browser for Android"
                ua  = "Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T; wv) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/43.0.2357.121 Mobile Safari/537.36 [FB_IAB/FB4A;FBAV/35.0.0.48.273;]"
                expect  =
                   {
                    name = Some"Facebook"
                    version = Some "35.0.0.48.273"
                    //"major"   : "35"
                }
            }
            {
                desc = "Facebook in-App Browser for iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_1 like Mac OS X) AppleWebKit/603.1.30 (KHTML like Gecko) Mobile/14E304 [FBAN/FBIOS;FBAV/91.0.0.41.73;FBBV/57050710;FBDV/iPhone81;FBMD/iPhone;FBSN/iOS;FBSV/10.3.1;FBSS/2;FBCR/Telekom.de;FBID/phone;FBLC/de_DE;FBOP/5;FBRV/0])"
                expect  =
                   {
                    name = Some"Facebook"
                    version = Some "91.0.0.41.73"
                    //"major"   : "91"
                }
            }
            {
                desc = "Falkon"
                ua  = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML like Gecko) Falkon/3.0.0 Chrome/61.0.3163.140 Safari/537.36"
                expect  =
                   {
                    name = Some"Falkon"
                    version = Some "3.0.0"
                    //"major"   : "3"
                }
            }
            {
                desc = "Firebird"
                ua  = "Mozilla/5.0 (Windows; U; Win98; en-US; rv:1.5) Gecko/20031007 Firebird/0.7"
                expect  =
                   {
                    name = Some"Firebird"
                    version = Some "0.7"
                    //"major"   : "0"
                }
            }
            {
                desc = "Firefox"
                ua  = "Mozilla/5.0 (Windows NT 6.1; rv:15.0) Gecko/20120716 Firefox/15.0a2"
                expect  =
                   {
                    name = Some"Firefox"
                    version = Some "15.0a2"
                    //"major"   : "15"
                }
            }
            {
                desc = "Fennec"
                ua  = "Mozilla/5.0 (X11; U; Linux armv61; en-US; rv:1.9.1b2pre) Gecko/20081015 Fennec/1.0a1"
                expect  =
                   {
                    name = Some"Fennec"
                    version = Some "1.0a1"
                    //"major"   : "1"
                }
            }
            {
                desc = "Firefox Focus"
                ua = "Mozilla/5.0 (Linux; Android 7.0) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Focus/6.1.1 Chrome/68.0.3440.91 Mobile Safari/537.36"
                expect = {
                    name = Some "Firefox Focus"
                    version = Some "6.1.1"
                    //"major": "6"
                }
            }
            {
                desc = "Flock"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.3) Gecko/2008100716 Firefox/3.0.3 Flock/2.0"
                expect  =
                   {
                    name = Some"Flock"
                    version = Some "2.0"
                    //"major"   : "2"
                }
            }
            {
                desc = "GoBrowser"
                ua  = "Nokia5700XpressMusic/GoBrowser/1.6.91"
                expect  =
                   {
                    name = Some"GoBrowser"
                    version = Some "1.6.91"
                    //"major"   : "1"
                }
            }
            {
                desc = "IceApe"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1.19) Gecko/20110817 Iceape/2.0.14"
                expect  =
                   {
                    name = Some"Iceape"
                    version = Some "2.0.14"
                    //"major"   : "2"
                }
            }
            {
                desc = "IceCat"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.3) Gecko/2008092921 IceCat/3.0.3-g1"
                expect  =
                   {
                    name = Some"IceCat"
                    version = Some "3.0.3-g1"
                    //"major"   : "3"
                }
            }
            {
                desc = "Iceweasel"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; de; rv:1.9.0.16) Gecko/2009121610 Iceweasel/3.0.6 (Debian-3.0.6-3)"
                expect  =
                   {
                    name = Some"Iceweasel"
                    version = Some "3.0.6"
                    //"major"   : "3"
                }
            }
            {
                desc = "iCab"
                ua  = "iCab/2.9.5 (Macintosh; U; PPC; Mac OS X)"
                expect  =
                   {
                    name = Some"iCab"
                    version = Some "2.9.5"
                    //"major"   : "2"
                }
            }
            {
                desc = "IEMobile"
                ua  = "Mozilla/4.0 (compatible; MSIE 6.0; Windows CE; IEMobile 7.11) 320x240; VZW; Motorola-Q9c; Windows Mobile 6.1 Standard"
                expect  =
                   {
                    name = Some"IEMobile"
                    version = Some "7.11"
                    //"major"   : "7"
                }
            }
            {
                desc = "IE 11 with IE token"
                ua  = "Mozilla/5.0 (IE 11.0; Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko"
                expect  =
                   {
                    name = Some"IE"
                    version = Some "11.0"
                    //"major"   : "11"
                }
            }
            {
                desc = "IE 11 without IE token"
                ua  = "Mozilla/5.0 (Windows NT 6.3; Trident/7.0; rv 11.0) like Gecko"
                expect  =
                   {
                    name = Some"IE"
                    version = Some "11.0"
                    //"major"   : "11"
                }
            }
            {
                desc = "K-Meleon"
                ua  = "Mozilla/5.0 (Windows; U; Win98; en-US; rv:1.5) Gecko/20031016 K-Meleon/0.8.2"
                expect  =
                   {
                    name = Some"K-Meleon"
                    version = Some "0.8.2"
                    //"major"   : "0"
                }
            }
            {
                desc = "Kindle Browser"
                ua  = "Mozilla/4.0 (compatible; Linux 2.6.22) NetFront/3.4 Kindle/2.5 (screen 600x800; rotate)"
                expect  =
                   {
                    name = Some"Kindle"
                    version = Some "2.5"
                    //"major"   : "2"
                }
            }
            {
                desc = "Konqueror"
                ua  = "Mozilla/5.0 (compatible; Konqueror/3.5; Linux; X11; x86_64) KHTML/3.5.6 (like Gecko) (Kubuntu)"
                expect  =
                   {
                    name = Some"Konqueror"
                    version = Some "3.5"
                    //"major"   : "3"
                }
            }
            {
                desc = "Konqueror"
                ua  = "Mozilla/5.0 (X11; Linux i686) AppleWebKit/534.34 (KHTML like Gecko) konqueror/5.0.97 Safari/534.34"
                expect  =
                   {
                    name = Some"Konqueror"
                    version = Some "5.0.97"
                    //"major"   : "5"
                }
            }
            {
                desc = "LINE on Android"
                ua  = "Mozilla/5.0 (Linux; Android 5.0; ASUS_Z00AD Build/LRX21V; wv) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/51.0.2704.81 Mobile Safari/537.36 Line/6.5.1/IAB"
                expect  =
                   {
                    name = Some"Line"
                    version = Some "6.5.1"
                    //"major"   : "6"
                }
            }
            {
                desc = "LINE on iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_2_6 like Mac OS X) AppleWebKit/604.5.6 (KHTML like Gecko) Mobile/15D100 Safari Line/8.4.1"
                expect  =
                   {
                    name = Some"Line"
                    version = Some "8.4.1"
                    //"major"   : "8"
                }
            }
            {
                desc = "Lunascape"
                ua  = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.2) Gecko/20090804 Firefox/3.5.2 Lunascape/5.1.4.5"
                expect  =
                   {
                    name = Some"Lunascape"
                    version = Some "5.1.4.5"
                    //"major"   : "5"
                }
            }
            {
                desc = "Lynx"
                ua  = "Lynx/2.8.5dev.16 libwww-FM/2.14 SSL-MM/1.4.1 OpenSSL/0.9.6b"
                expect  =
                   {
                    name = Some"Lynx"
                    version = Some "2.8.5dev.16"
                    //"major"   : "2"
                }
            }
            {
                desc = "Maemo Browser"
                ua  = "Mozilla/5.0 (X11; U; Linux armv7l; ru-RU; rv:1.9.2.3pre) Gecko/20100723 Firefox/3.5 Maemo Browser 1.7.4.8 RX-51 N900"
                expect  =
                   {
                    name = Some"Maemo Browser"
                    version = Some "1.7.4.8"
                    //"major"   : "1"
                }
            }
            {
                desc = "Maxthon"
                ua  = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; SV1; Maxthon; .NET CLR 1.1.4322)"
                expect  =
                   {
                    name = Some"Maxthon"
                    version = None
                    //"major"   : "undefined"
                }
            }
            {
                desc = "Midori"
                ua  = "Midori/0.2.2 (X11; Linux i686; U; en-us) WebKit/531.2+"
                expect  =
                   {
                    name = Some"Midori"
                    version = Some "0.2.2"
                    //"major"   : "0"
                }
            }
            {
                desc = "Minimo"
                ua  = "Mozilla/5.0 (X11; U; Linux armv6l; rv 1.8.1.5pre) Gecko/20070619 Minimo/0.020"
                expect  =
                   {
                    name = Some"Minimo"
                    version = Some "0.020"
                    //"major"   : "0"
                }
            }
            {
                desc = "MIUI Browser on Xiaomi Hongmi WCDMA (HM2013023)"
                ua  = "Mozilla/5.0 (Linux; U; Android 4.2.2; ru-ru; 2013023 Build/HM2013023) AppleWebKit/534.30 (KHTML like Gecko) Version/4.0 Mobile Safari/534.30 XiaoMi/MiuiBrowser/1.0"
                expect  =
                   {
                    name = Some"MIUI Browser"
                    version = Some "1.0"
                    //"major"   : "1"
                }
            }
            {
                desc = "Mobile Safari"
                ua  = "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7"
                expect  =
                   {
                    name = Some"Mobile Safari"
                    version = Some "4.0.5"
                    //"major"   : "4"
                }
            }
            {
                desc = "Mosaic"
                ua  = "NCSA_Mosaic/2.6 (X11; SunOS 4.1.3 sun4m)"
                expect  =
                   {
                    name = Some"Mosaic"
                    version = Some "2.6"
                    //"major"   : "2"
                }
            }
            {
                desc = "Mozilla"
                ua  = "Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.7) Gecko/20070606"
                expect  =
                   {
                    name = Some"Mozilla"
                    version = Some "5.0"
                    //"major"   : "5"
                }
            }
            {
                desc = "MSIE"
                ua  = "Mozilla/4.0 (compatible; MSIE 5.0b1; Mac_PowerPC)"
                expect  =
                   {
                    name = Some"IE"
                    version = Some "5.0b1"
                    //"major"   : "5"
                }
            }
            {
                desc = "NetFront"
                ua  = "Mozilla/4.0 (PDA; Windows CE/1.0.1) NetFront/3.0"
                expect  =
                   {
                    name = Some"NetFront"
                    version = Some "3.0"
                    //"major"   : "3"
                }
            }
            {
                desc = "Netscape on Windows ME"
                ua  = "Mozilla/5.0 (Windows; U; Win 9x 4.90; en-US; rv:1.8.1.8pre) Gecko/20071015 Firefox/2.0.0.7 Navigator/9.0"
                expect  =
                   {
                    name = Some"Netscape"
                    version = Some "9.0"
                    //"major"   : "9"
                }
            }
            {
                desc = "Netscape on Windows 2000"
                ua  = "Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.7.5) Gecko/20050519 Netscape/8.0.1"
                expect  =
                   {
                    name = Some"Netscape"
                    version = Some "8.0.1"
                    //"major"   : "8"
                }
            }
            {
                desc = "Nokia Browser"
                ua  = "Mozilla/5.0 (Symbian/3; Series60/5.2 NokiaN8-00/025.007; Profile/MIDP-2.1 Configuration/CLDC-1.1 ) AppleWebKit/533.4 (KHTML like Gecko) NokiaBrowser/7.3.1.37 Mobile Safari/533.4 3gpp-gba"
                expect  =
                   {
                    name = Some"NokiaBrowser"
                    version = Some "7.3.1.37"
                    //"major"   : "7"
                }
            }
            {
                desc = "Oculus Browser"
                ua  = "Mozilla/5.0 (Linux; Android 7.0; SM-G920I Build/NRD90M) AppleWebKit/537.36 (KHTML like Gecko) OculusBrowser/3.4.9 SamsungBrowser/4.0 Chrome/57.0.2987.146 Mobile VR Safari/537.36"
                expect  =
                   {
                    name = Some"Oculus Browser"
                    version = Some "3.4.9"
                    //"major"   : "3"
                }
            }
            {
                desc = "OmniWeb"
                ua  = "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en-US) AppleWebKit/85 (KHTML like Gecko) OmniWeb/v558.48"
                expect  =
                   {
                    name = Some"OmniWeb"
                    version = Some "558.48"
                    //"major"   : "558"
                }
            }
            {
                desc = "Opera > 9.80"
                ua  = "Opera/9.80 (X11; Linux x86_64; U; Linux Mint; en) Presto/2.2.15 Version/10.10"
                expect  =
                   {
                    name = Some"Opera"
                    version = Some "10.10"
                    //"major"   : "10"
                }
            }
            {
                desc = "Opera < 9.80 on Windows"
                ua  = "Mozilla/4.0 (compatible; MSIE 5.0; Windows 95) Opera 6.01 [en]"
                expect  =
                   {
                    name = Some"Opera"
                    version = Some "6.01"
                    //"major"   : "6"
                }
            }
            {
                desc = "Opera < 9.80 on OSX"
                ua  = "Opera/8.5 (Macintosh; PPC Mac OS X; U; en)"
                expect  =
                   {
                    name = Some"Opera"
                    version = Some "8.5"
                    //"major"   : "8"
                }
            }
            {
                desc = "Opera Mobile"
                ua  = "Opera/9.80 (Android 2.3.5; Linux; Opera Mobi/ADR-1111101157; U; de) Presto/2.9.201 Version/11.50"
                expect  =
                   {
                    name = Some"Opera Mobi"
                    version = Some "11.50"
                    //"major"   : "11"
                }
            }
            {
                desc = "Opera Webkit"
                ua  = "Mozilla/5.0 AppleWebKit/537.22 (KHTML like Gecko) Chrome/25.0.1364.123 Mobile Safari/537.22 OPR/14.0.1025.52315"
                expect  =
                   {
                    name = Some"Opera"
                    version = Some "14.0.1025.52315"
                    //"major"   : "14"
                }
            }
            {
                desc = "Opera Mini"
                ua  = "Opera/9.80 (J2ME/MIDP; Opera Mini/5.1.21214/19.916; U; en) Presto/2.5.25"
                expect  =
                   {
                    name = Some"Opera Mini"
                    version = Some "5.1.21214"
                    //"major"   : "5"
                }
            }
            {
                desc = "Opera Mini 8 above on iPhone"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 9_2 like Mac OS X) AppleWebKit/601.1.46 (KHTML like Gecko) OPiOS/12.1.1.98980 Mobile/13C75 Safari/9537.53"
                expect  =
                   {
                    name = Some"Opera Mini"
                    version = Some "12.1.1.98980"
                    //"major"   : "12"
                }
            }
            {
                desc = "Opera Tablet"
                ua  = "Opera/9.80 (Windows NT 6.1; Opera Tablet/15165; U; en) Presto/2.8.149 Version/11.1"
                expect  =
                   {
                    name = Some"Opera Tablet"
                    version = Some "11.1"
                    //"major"   : "11"
                }
            }
            {
                desc = "Opera Coast"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 9_3_2 like Mac OS X; en) AppleWebKit/601.1.46 (KHTML like Gecko) Coast/5.04.110603 Mobile/13F69 Safari/7534.48.3"
                expect  =
                   {
                    name = Some"Opera Coast"
                    version = Some "5.04.110603"
                    //"major"   : "5"
                }
            }
            {
                desc = "Opera Touch"
                ua  = "Mozilla/5.0 (Linux; Android 7.0; Lenovo P2a42 Build/NRD90N) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/68.0.3440.91 Mobile Safari/537.36 OPT/1.10.33"
                expect  =
                   {
                    name = Some"Opera Touch"
                    version = Some "1.10.33"
                    //"major"   : "1"
                }
            }
            {
                desc = "PhantomJS"
                ua  = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/534.34 (KHTML like Gecko) PhantomJS/1.9.2 Safari/534.34"
                expect  =
                   {
                    name = Some"PhantomJS"
                    version = Some "1.9.2"
                    //"major"   : "1"
                }
            }
            {
                desc = "Phoenix"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.2b) Gecko/20021029 Phoenix/0.4"
                expect  =
                   {
                    name = Some"Phoenix"
                    version = Some "0.4"
                    //"major"   : "0"
                }
            }
            {
                desc = "Polaris"
                ua  = "LG-LX600 Polaris/6.0 MMP/2.0 Profile/MIDP-2.1 Configuration/CLDC-1.1"
                expect  =
                   {
                    name = Some"Polaris"
                    version = Some "6.0"
                    //"major"   : "6"
                }
            }
            {
                desc = "QQ"
                ua  = "Mozilla/5.0 (Linux; U; Android 4.4.4; zh-cn; OPPO R7s Build/KTU84P) AppleWebKit/537.36 (KHTML like Gecko)Version/4.0 Chrome/37.0.0.0 MQQBrowser/7.1 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"QQBrowser"
                    version = Some "7.1"
                    //"major"   : "7"
                }
            }
            {
                desc = "QupZilla"
                ua  = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/538.1 (KHTML like Gecko) QupZilla/1.8.9 Safari/538.1"
                expect  =
                   {
                    name = Some"QupZilla"
                    version = Some "1.8.9"
                    //"major"   : "1"
                }
            }
            {
                desc = "RockMelt"
                ua  = "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.7 (KHTML like Gecko) RockMelt/0.8.36.78 Chrome/7.0.517.44 Safari/534.7"
                expect  =
                   {
                    name = Some"RockMelt"
                    version = Some "0.8.36.78"
                    //"major"   : "0"
                }
            }
            {
                desc = "Safari"
                ua  = "Mozilla/5.0 (Windows; U; Windows NT 5.2; en-US) AppleWebKit/533.17.8 (KHTML like Gecko) Version/5.0.1 Safari/533.17.8"
                expect  =
                   {
                    name = Some"Safari"
                    version = Some "5.0.1"
                    //"major"   : "5"
                }
            }
            {
                desc = "Safari < 3.0"
                ua  = "Mozilla/5.0 (Macintosh; U; PPC Mac OS X; sv-se) AppleWebKit/419 (KHTML like Gecko) Safari/419.3"
                expect  =
                   {
                    name = Some"Safari"
                    version = Some "2.0.4"
                    //"major"   : "2"
                }
            }
            {
                desc = "Samsung Browser"
                ua  = "Mozilla/5.0 (Linux; Android 6.0.1; SAMSUNG-SM-G925A Build/MMB29K) AppleWebKit/537.36 (KHTML like Gecko) SamsungBrowser/4.0 Chrome/44.0.2403.133 Mobile Safari/537.36"
                expect  =
                   {
                    name = Some"Samsung Browser"
                    version = Some "4.0"
                    //"major"   : "4"
                }
            }
            {
                desc = "SeaMonkey"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.1b4pre) Gecko/20090405 SeaMonkey/2.0b1pre"
                expect  =
                   {
                    name = Some"SeaMonkey"
                    version = Some "2.0b1pre"
                    //"major"   : "2"
                }
            }
            {
                desc = "Silk Browser"
                ua  = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_3; en-us; Silk/1.1.0-84)"
                expect  =
                   {
                    name = Some"Silk"
                    version = Some "1.1.0-84"
                    //"major"   : "1"
                }
            }
            {
                desc = "Skyfire"
                ua  = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_5_7; en-us) AppleWebKit/530.17 (KHTML like Gecko) Version/4.0 Safari/530.17 Skyfire/2.0"
                expect  =
                   {
                    name = Some"Skyfire"
                    version = Some "2.0"
                    //"major"   : "2"
                }
            }
            {
                desc = "SlimBrowser"
                ua  = "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; SlimBrowser)"
                expect  =
                   {
                    name = Some"Slim"
                    version = None
                    //"major"   : "undefined"
                }
            }
            {
                desc = "Swiftfox"
                ua  = "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1) Gecko/20061024 Firefox/2.0 (Swiftfox)"
                expect  =
                   {
                    name = Some"Swiftfox"
                    version = None
                    //"major"   : "undefined"
                }
            }
            {
                desc = "Tizen Browser"
                ua  = "Mozilla/5.0 (Linux; U; Tizen/1.0 like Android; en-us; AppleWebKit/534.46 (KHTML like Gecko) Tizen Browser/1.0 Mobile"
                expect  =
                   {
                    name = Some"Tizen Browser"
                    version = Some "1.0"
                    //"major"   : "1"
                }
            }
            {
                desc = "UC Browser"
                ua  = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/54.0.2840.99 UBrowser/5.6.12860.7 Safari/537.36"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "5.6.12860.7"
                    //"major"   : "5"
                }
            }
            {
                desc = "UC Browser"
                ua  = "Mozilla/5.0 (Linux; U; Android 6.0.1; en-US; Lenovo P2a42 Build/MMB29M) AppleWebKit/534.30 (KHTML like Gecko) Version/4.0 UCBrowser/11.2.0.915 U3/0.8.0 Mobile Safari/534.30"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "11.2.0.915"
                    //"major"   : "11"
                }
            }
            {
                desc = "UC Browser on Samsung"
                ua  = "Mozilla/5.0 (Java; U; Pt-br; samsung-gt-s5620) UCBrowser8.2.1.144/69/352/UCWEB Mobile UNTRUSTED/1.0"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "8.2.1.144"
                    //"major"   : "8"
                }
            }
            {
                desc = "UC Browser on Nokia"
                ua  = "Mozilla/5.0 (S60V3; U; en-in; NokiaN73)/UC Browser8.4.0.159/28/351/UCWEB Mobile"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "8.4.0.159"
                    //"major"   : "8"
                }
            }
            {
                desc = "UC Browser J2ME"
                ua  = "UCWEB/2.0 (MIDP-2.0; U; zh-CN; HTC EVO 3D X515m) U2/1.0.0 UCBrowser/10.4.0.558 U2/1.0.0 Mobile"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "10.4.0.558"
                    //"major"   : "10"
                }
            }
            {
                desc = "UC Browser J2ME 2"
                ua  = "JUC (Linux; U; 2.3.5; zh-cn; GT-I9100; 480*800) UCWEB7.9.0.94/139/800"
                expect  =
                   {
                    name = Some"UCBrowser"
                    version = Some "7.9.0.94"
                    //"major"   : "7"
                }
            }
            {
                desc = "WeChat on iOS"
                ua = "Mozilla/5.0 (iPhone; CPU iPhone OS 8_4_1 like Mac OS X) AppleWebKit/600.1.4 (KHTML like Gecko) Mobile/12H321 MicroMessenger/6.3.6 NetType/WIFI Language/zh_CN"
                expect =
                   {
                    name = Some "WeChat"
                    version = Some "6.3.6"
                    //"major": "6"
                }
            }
            {
                desc = "WeChat on Android"
                ua = "Mozilla/5.0 (Linux; U; Android 5.1; zh-cn; Lenovo K50-t5 Build/LMY47D) AppleWebKit/533.1 (KHTML like Gecko)Version/4.0 MQQBrowser/5.4 TBS/025478 Mobile Safari/533.1 MicroMessenger/6.3.5.50_r1573191.640 NetType/WIFI Language/zh_CN"
                expect =
                   {
                    name = Some "WeChat"
                    version = Some "6.3.5.50_r1573191.640"
                    //"major": "6"
                }
            }
            {
                desc = "Vivaldi"
                ua  = "Mozilla/5.0 (Windows NT 6.0) AppleWebKit/537.36 (KHTML like Gecko) Chrome/40.0.2214.89 Vivaldi/1.0.83.38 Safari/537.36"
                expect  =
                   {
                    name = Some"Vivaldi"
                    version = Some "1.0.83.38"
                    //"major"   : "1"
                }
            }
            {
                desc = "Yandex"
                ua  = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/536.5 (KHTML like Gecko) YaBrowser/1.0.1084.5402 Chrome/19.0.1084.5402 Safari/536.5"
                expect  =
                   {
                    name = Some"Yandex"
                    version = Some "1.0.1084.5402"
                    //"major"   : "1"
                }
            }
            {
                desc = "Puffin"
                ua  = "Mozilla/5.0 (Linux; Android 6.0.1; Lenovo P2a42 Build/MMB29M; en-us) AppleWebKit/537.36 (KHTML like Gecko) Chrome/42.0.2311.135 Mobile Safari/537.36 Puffin/6.0.8.15804AP"
                expect  =
                   {
                    name = Some"Puffin"
                    version = Some "6.0.8.15804AP"
                    //"major"   : "6"
                }
            }
            {
                desc = "Microsoft Edge"
                ua  = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/39.0.2171.71 Safari/537.36 Edge/12.0"
                expect  =
                   {
                    name = Some"Edge"
                    version = Some "12.0"
                    //"major"   : "12"
                }
            }
            {
                desc = "Microsoft Edge on iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 11_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML like Gecko) Version/11.0 EdgiOS/42.1.1.0 Mobile/15F79 Safari/605.1.15"
                expect  =
                   {
                    name = Some"Edge"
                    version = Some "42.1.1.0"
                    //"major"   : "42"
                }
            }
            {
                desc = "Microsoft Edge on Android"
                ua  = "Mozilla/5.0 (Linux; Android 8.0.0; G8441 Build/47.1.A.12.270) AppleWebKit/537.36 (KHTML like Gecko) Chrome/67.0.3396.123 Mobile Safari/537.36 EdgA/42.0.0.2529"
                expect  =
                   {
                    name = Some"Edge"
                    version = Some "42.0.0.2529"
                    //"major"   : "42"
                }
            }
            {
                desc = "Iridium"
                ua  = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML like Gecko) Iridium/43.8 Safari/537.36 Chrome/43.0.2357.132"
                expect  =
                   {
                    name = Some"Iridium"
                    version = Some "43.8"
                    //"major"   : "43"
                }
            }
            {
                desc = "Firefox iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 9_1 like Mac OS X) AppleWebKit/601.1.46 (KHTML like Gecko) FxiOS/1.1 Mobile/13B143 Safari/601.1.46"
                expect  =
                   {
                    name = Some"Firefox"
                    version = Some "1.1"
                    //"major"   : "1"
                }
            }
            {
                desc = "QQ on iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_0_2 like Mac OS X) AppleWebKit/602.1.50 (KHTML like Gecko) Mobile/14A456 QQ/6.5.3.410 V1_IPH_SQ_6.5.3_1_APP_A Pixel/1080 Core/UIWebView NetType/WIFI Mem/26"
                expect  =
                   {
                    name = Some"QQ"
                    version = Some "6.5.3.410"
                    //"major"   : "6"
                }
            }
            {
                desc = "QQ on Android"
                ua  = "Mozilla/5.0 (Linux; Android 6.0; PRO 6 Build/MRA58K) AppleWebKit/537.36 (KHTML like Gecko) Version/4.0 Chrome/37.0.0.0 Mobile MQQBrowser/6.8 TBS/036824 Safari/537.36 V1_AND_SQ_6.5.8_422_YYB_D PA QQ/6.5.8.2910 NetType/WIFI WebP/0.3.0 Pixel/1080"
                expect  =
                   {
                    name = Some"QQ"
                    version = Some "6.5.8.2910"
                    //"major"   : "6"
                }
            }
            {
                desc = "GSA on iOS"
                ua  = "Mozilla/5.0 (iPhone; CPU iPhone OS 10_3_2 like Mac OS X) AppleWebKit/602.1.50 (KHTML like Gecko) GSA/30.1.161623614 Mobile/14F89 Safari/602.1"
                expect  =
                   {
                    name = Some"GSA"
                    version = Some "30.1.161623614"
                    //"major"   : "30"
                }
            }
            {
                desc = "Sogou Browser"
                ua  = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X MetaSr 1.0"
                expect  =
                   {
                    name = Some"MetaSr"
                    version = Some "1.0"
                }
            }
            {
                desc = "LieBao Browser"
                ua  = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/42.0.2311.154 Safari/537.36 LBBROWSER"
                expect  =
                   {
                    name = Some"LBBROWSER"
                    version = None
                }
            }
            {
                desc = "BaiDu Browser"
                ua  = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/47.0.2526.106 BIDUBrowser/8.7 Safari/537.36"
                expect  =
                   {
                    name = Some"BIDUBrowser"
                    version = Some "8.7" 
                    //"major"   : "8"
                }
            }
            {
                desc = "2345 Browser"
                ua  = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML like Gecko) Chrome/56.0.2924.90 Safari/537.36 2345Explorer/9.2.1.17116"
                expect  =
                   {
                    name = Some"2345Explorer"
                    version = Some "9.2.1.17116"
                    //"major"   : "9"
                }
            }
            {
                desc = "QQBrowserLite"
                ua  = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML like Gecko) Version/10.0.1 Safari/602.2.14 QQBrowserLite/1.1.0"
                expect  =
                   {
                    name = Some"QQBrowserLite"
                    version = Some "1.1.0"
                    //"major"   : "1"
                }
            }
            {
                desc = "Brave Browser"
                ua  = "Brave/4.5.16 CFNetwork/893.13.1 Darwin/17.3.0 (x86_64)"
                expect  =
                   {
                    name = Some"Brave"
                    version = Some "4.5.16"
                    //"major"   : "4"
                }
            }
]
