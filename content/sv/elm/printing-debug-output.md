---
title:                "Elm: Utskrift av felsökningsinformation"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en process fylld av utmaningar och problem som måste lösas. Ibland kan det vara svårt att förstå vad som händer i koden och varför vissa fel inträffar. Att skriva ut debug-utdata kan hjälpa till att förbättra förståelsen av koden och effektivisera felsökningen.

## Hur man gör

För att skriva ut debug-utdata i Elm, använd funktionen `Debug.log` som tar emot en sträng och ett värde som ska skrivas ut. Till exempel:

```Elm
Debug.log "Värde" 5
```

Detta kommer att skriva ut "Värde: 5" i konsolen när programmet körs.

Man kan även göra det mer dynamiskt genom att skriva ut värden från variabler eller funktioner:

```Elm
Debug.log "Variabel" minVariabel
Debug.log "Funktion" (minFunktion argument)
```

Det är också möjligt att kombinera flera värden i en sträng:

```Elm
Debug.log "Värden" ("Var1: " ++ var1 ++ ", Var2: " ++ var2)
```

Detta kommer att skriva ut "Värden: Var1: värde1, Var2: värde2".

## Djupdykning

En viktig aspekt av att skriva debug-utdata är att hitta den rätta balansen mellan att ha tillräckligt med information för att förstå koden och att inte överväldigas av för mycket information. Det är viktigt att vara selektiv och noggrann med vilka uttryck som väljs för att skriva ut.

En annan användbar funktion för debug-utdata är `Debug.todo` som används för att markera ställen i koden som behöver mer arbete eller implementering. Till exempel:

```Elm
todo "Fixa detta senare"
```

Detta är ett sätt att ge sig själv eller andra programmerare en påminnelse om att återkomma till det här stället i koden.

Att använda debugger-verktyget i din webbläsare kan också vara en användbar metod för att få mer detaljerad information om vad som händer i koden. Detta kan hjälpa till att identifiera och lösa problem som inte kan lösas med debug-utdata.

## Se också

- [Elm Documentation - Debug](https://elm-lang.org/docs/debug)
- [Debugging Elm](https://medium.com/@anne_mit_m/elm-debugging-2a73da9dcb2c)
- [Debugging in Elm](https://www.elm-tutorial.org/sv/03_debugging/00_introduction.html)