---
title:    "Elm: Utskrift av felsökningsutdata"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att skriva ut felsökningsutdata är en viktig del av programmering, oavsett vilket språk du använder. Det kan hjälpa dig att hitta och fixa fel i din kod snabbare och mer effektivt. I detta inlägg ska vi titta närmare på hur man skriver ut debug-utdata i Elm program.

## Hur man gör

För att skriva ut debug-utdata i ditt Elm program behöver du använda funktionen `Debug.log`. För att använda den kan du följa dessa steg:

1. Importera Debug modulen `import Debug`

2. Lägg till `Debug.log` funktionen i din kod och tilldela den till en variabel:

   ```elm
   log : String -> a -> a 
   ```

3. Använd funktionen `log` för att skriva ut din önskade utdata till konsolen:

   ```elm
   log "Min debug-utdata" "Viktig information"
   --Output: Min debug-utdata: "Viktig information"
   ```

4. För att använda funktionen behöver du också aktivera debug-utdata i din Elm-sidans utvecklingsläge. Detta kan du enkelt göra genom att lägga till `--debug` flaggan när du köra `elm reactor` eller `elm make` kommandon.

## Djupdykning

I Elm är debug-utdata en viktig del av felsökningen eftersom språket inte tillåter typfel och har strikt typning. Detta kan göra det svårt att hitta fel när de uppstår. Genom att använda debug-utdata kan du inspektera variabler och värden för att förstå vad som händer i din kod.

Det finns också flera olika sätt att formatera din debug-utdata i Elm, som att använda `Debug.toString` funktionen för att konvertera värden till strängar eller `Debug.todo` för att markera en viss del av din kod som ofullständig. Genom att experimentera med dessa olika alternativ kan du hitta det som passar bäst för dina felsökningsbehov.

## Se också

- Elm guide för felsökning: https://guide.elm-lang.org/debugging/
- Officiell Elm dokumentation för Debug modulen: https://package.elm-lang.org/packages/elm/core/latest/Debug