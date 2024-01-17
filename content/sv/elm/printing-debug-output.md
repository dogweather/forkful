---
title:                "Utskrift av felsökningsutdata"
html_title:           "Elm: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi programmerar är det inte alltid lätt att förstå vad som händer i koden. Det är här printfelsökning kommer in i bilden. Det är en metod för att skriva ut olika delar av koden i konsolen för att få en bättre förståelse för vad som händer när koden körs. Detta kan vara särskilt användbart när du försöker lösa buggar eller förstå en komplex algoritm.

## Hur man:
Elm har en inbyggd funktion för printfelsökning som heter `Debug.log`. Här är ett exempel på hur du kan använda den:

```Elm
-- Kodexempel
import Debug exposing (log)

-- Funktion som tar emot två heltal och returnerar deras summa
sum : Int -> Int -> Int
sum x y = 
  let
    result = x + y
  in
    Debug.log "Resultatet av summorna är:" result
```

Outputen i konsolen kommer att se ut som följande:

```Elm
Resultatet av summorna är: 15
```

Som du kan se har värdet av `result` skrivits ut i terminalen. Detta kan hjälpa dig att förstå vad som händer i koden. Du kan också använda `Debug.log` för att skriva ut andra värden, som till exempel listor eller strängar.

## Djupdykning:
Printfelsökning har funnits i många programmeringsspråk sedan lång tid tillbaka, och är ett enkelt men effektivt felsökningsverktyg. Men det finns också andra sätt att felsöka koden, till exempel genom att använda en debugger eller genom att skriva ut felmeddelanden vid exception. Vad som fungerar bäst för dig beror på dina personliga preferenser och projektets behov.

När det kommer till implementation så fungerar `Debug.log` genom att lägga till en extra parameter i funktionen som returnerar värdet. Detta gör att värdet kan skrivas ut i konsolen, men har ingen påverkan på själva funktionen i sig.

## Se även:
- [Officiell dokumentation för Debug-modulen i Elm](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [En guide för felsökning i Elm](https://medium.com/elm-shorts/debugging-elm-2768fbd6e939) 
- [En jämförelse mellan printfelsökning och debugger i Elm](https://qfpl.io/posts/practical-debugging-in-elm/)