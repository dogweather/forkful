---
title:                "Utskrift av felrapportering"
html_title:           "Elm: Utskrift av felrapportering"
simple_title:         "Utskrift av felrapportering"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

För många utvecklare är debuggning en viktig del av programmeringsprocessen. Det är ett sätt att förstå hur ens kod fungerar och hitta eventuella fel eller buggar som behöver åtgärdas. Genom att använda debug-utskrifter kan du enkelt få en bättre förståelse för dina program och dess olika delar.

## Hur man gör

För att skriva ut debug-information i Elm, kan du använda funktionen `Debug.log` som ingår i standardbiblioteket. Den låter dig skriva ut värden på ett enkelt och effektivt sätt. Här är ett exempel på hur du kan använda den:

```Elm
elm repl
> import Debug
> x = 5
> Debug.log "Värdet på x är" x
Värdet på x är 5 : number
```

Som du kan se ovanför behöver vi importera `Debug`-paketet för att använda funktionen. Sedan kan vi definiera en variabel och sedan använda `Debug.log` för att skriva ut värdet av variabeln tillsammans med en beskrivning. I detta fall skriver vi ut "Värdet på x är" och sedan variabelns värde.

Om du kör detta i en vanlig fil, används `elm reactor` istället. Resultatet kommer att visas i din webbläsare och du kan öppna webbkonsolen för att se utskriften.

Debuggning via utskrifter kan också användas i mer komplexa situationer, till exempel när du behöver undersöka en lista av värden. Här är ett annat exempel på hur du kan använda `Debug.log`:

```Elm
elm repl
> import Debug
> lista = [1, 2, 3]
> listaSquared = List.map (\x -> x * x) lista
> Debug.log "Den ursprungliga listan är" lista ++ Debug.log "Den kvadrerade listan är" listaSquared
Den ursprungliga listan är [1,2,3] : List number
Den kvadrerade listan är [1,4,9] : List number
```

Som du kan se, kan du använda `Debug.log` flera gånger i samma uttryck. Detta gör det enkelt att jämföra värden och förstå vad som händer vid varje steg.

## Djupdykning

Det är viktigt att komma ihåg att debug-utskrifter inte ska användas som en permanent lösning för att lösa fel eller buggar i din kod. Det är bara en tillfällig hjälp för att förstå vad som händer i ditt program. Därför är det bra att ta bort dem när du är klar med din debuggning för att inte påverka prestandan på dina slutliga program.

Ett annat bra tips är att använda modulnamn eller beskrivningar i dina debug-utskrifter för att enkelt kunna identifiera dem senare. Detta är särskilt användbart när du har flera debug-utskrifter utspridda i din kod.

## Se också

* [Elm Debug Docs](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
* [Debugging in Elm](https://medium.com/@thejameskyle/debugging-in-elm-a83b6d12aeb2)
* [Debugging Elm in the Browser with Breakpoints](https://javascript-tecnology-sajitha.blogspot.com/2018/06/debugging-elm-in-browser-with.html)