---
title:    "Haskell: Skrivning till standardfel"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error i Haskell är ett enkelt sätt att hantera felmeddelanden i dina program. Genom att skriva till standard error kan du tydligt separera dina felmeddelanden från annan utdata.

## Så här gör man
För att skriva till standard error i Haskell behöver du importera modulen `System.IO` och använda funktionen `hPutStrLn` som tar två argument - ett Handle och en sträng.

```Haskell
import System.IO

hPutStrLn stderr "Det här är ett felmeddelande!"
```

Kör du detta program kommer du att se "Det här är ett felmeddelande!" på din standard error utdata.

```Haskell
import System.IO

main = do
    hPutStrLn stderr "Det här är ett felmeddelande!"
    putStrLn "Inte ett felmeddelande!"
```

I detta exempel ser vi hur standard error och standard output kan skiljas åt genom att använda `hPutStrLn` på respektive Handle.

## Djupdykning
När ett program körs är det oftast bra att använda sig av `putStrLn` för att skriva till standard output, eftersom det är där all vanlig utdata sker. Men om du vill ha en tydlig separering av felmeddelanden är det bra att använda `hPutStrLn` på standard error.

Det är också möjligt att byta ut standard error Handle mot en egen Handle, till exempel en fil. På så sätt kan du skriva felmeddelanden till en specifik fil för senare referens.

## Se även
- [Haskells dokumentation för System.IO](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [En tutorial för att skriva till standard error i Haskell](https://www.fpcomplete.com/blog/2011/07/fun-with-seha-fun-io-api-functions#putchar-and-puts)
- [Dagens blogginlägg i lite samma anda, fast för JavaScript](https://dev.to/godcrampy/using-the-console-error-method-in-javascript-2en0)