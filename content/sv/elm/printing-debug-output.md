---
title:    "Elm: Utskrift av felsökningsutdata"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är ofta en ganska ensam uppgift, och det kan vara svårt att förstå varför något inte fungerar som det ska. Ibland kan det vara till hjälp att ha lite extra information om vad som händer i koden för att felsöka och hitta fel snabbare. Det är här utskrift av debug-utdata kan vara till stor hjälp.

## Hur man gör det

För att skriva ut debug-utdata i Elm, kan du använda funktionen `Debug.log`. Det tar två argument - ett strängvärde som förklarar vad du vill skriva ut och ett värde som är det du faktiskt vill skriva ut. Här är ett exempel:

```Elm
import Debug exposing (log)

x = 5
y = 10

z = x + y

log "Värdet på z är:" z -- skriver ut "Värdet på z är: 15" i konsolen
```

I exemplet ovan förklarar strängen `"Värdet på z är:"` vad som kommer att skrivas ut och värdet `z` är det som faktiskt skrivs ut. Genom att använda `Debug.log` på viktiga punkter i koden kan du enkelt spåra vad som händer och hitta eventuella fel.

## Djupdykning

I Elm finns det också möjlighet att skriva ut mer avancerad utdata med hjälp av den inbyggda funktionen `Debug.toString`. Den konverterar ett värde till en strängrepresentation, vilket kan vara särskilt användbart för att skriva ut komplexa datastrukturer som listor eller tupler. Här är ett exempel:

```Elm
import Debug exposing (log)
import List exposing (map)

lista = [1, 2, 3]

kvadrater = map (\x -> x * x) lista

log "Lista med kvadrater:" (Debug.toString kvadrater) -- skriver ut "Lista med kvadrater: [1, 4, 9]" i konsolen
```

Som du kan se i exemplet ovan använder vi här `Debug.toString` för att konvertera värdet `kvadrater` till en strängrepresenation som vi sedan skriver ut med hjälp av `Debug.log`. Detta gör det enklare att se vad som händer med våra data när vi använder funktionen `map`.

## Se även

- [Elm guide om felsökning](https://guide.elm-lang.org/debugging/)
- [Officiell Elm-sida om Debug-modulen](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Artikel om felsökning med Elm](https://medium.com/@dannyfritz/a-comprehensive-guide-to-debugging-elm-a8a522efd6a4)