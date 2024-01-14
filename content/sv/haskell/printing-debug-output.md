---
title:    "Haskell: Utmatning av felsökningsinformation"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför
Att skriva kod i Haskell kan vara en utmaning, men med rätt verktyg och tekniker kan det bli mycket effektivare. En viktig del av det är att skriva ut debuggutdata för att underlätta felsökning och förståelse av programmet.

## Hur man gör det
Ett enkelt sätt att skriva ut debuggutdata är med funktionen `debug` från paketet `Debug.Trace`. Detta paket är en del av Haskell-plattformen, så det behöver inte installeras separat.

Det första steget är att importera paketet:

```Haskell
import Debug.Trace
```

Sedan kan vi använda `debug` genom att tillhandahålla en sträng med önskad utskrift och det värde som vi vill undersöka:

```Haskell
let x = 5
debug "Värdet på x är:" x
```

Detta kommer att skriva ut följande när koden körs:

```
Värdet på x är: 5
```

Vi kan också använda `debug` för att spela in flera värden, till exempel:

```Haskell
let y = 10
debug "Värdet på x är:" x
debug "Värdet på y är:" y
```

Vilket ger oss följande resultat:

```
Värdet på x är: 5
Värdet på y är: 10
```

## Djupdykning
För att göra debuggning ännu mer effektivt kan vi använda funktionen `traceShow` från samma paket. Denna funktion tar en sträng och ett värde som argument och returnerar samma värde, vilket gör det möjligt att använda den i uttryck.

Ett exempel på detta är:

```Haskell
let x = 5
let y = traceShow "Värdet på x är:" x + 5
```

Detta ger oss värdet 10 för y, men också utskriften `Värdet på x är: 5`. På så sätt kan vi enkelt spåra värdet på variabler i våra uttryck.

## Se även
- [Haskell-dokumentation för Debug.Trace](https://www.haskell.org/hoogle/?hoogle=debug)
- [En introduktion till Haskell-debuggning](https://www.fpcomplete.com/blog/2017/10/haskell-debugging-introduction)
- [Debugging i Haskell med ghci](https://mmhaskell.com/blog/2019/2/4/debugging-in-haskell-with-ghci)