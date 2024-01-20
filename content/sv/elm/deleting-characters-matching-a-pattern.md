---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är att identifiera och ta bort en sekvens av tecken baserat på ett specifikt mönster eller regel från en textsträng. Detta hjälper programmerare att manipulera data mer effektivt och rensa upp onödig information.

## Hur man gör:
Här är kodexempel och output på hur man utför detta i Elm.

```Elm
import Regex exposing (contains, regex)

deleteMatchingChars : String -> String -> String
deleteMatchingChars pattern text = 
  let
    re = regex pattern
  in
    if contains re text then
      String.replace pattern "" text
    else
      text
```
Låt oss prova det här med följande:
```Elm
deleteMatchingChars "choco" "I love chocolate"
```
Output:
```Elm
"I love late"
```
## Fördjupning
Radera tecken baserat på matchande mönster spårar tillbaka till de tidiga dagarna av programmering där datamanipulering var nödvändig för att optimera minnesanvändning.

Elm utelämnar inbyggda metoder för textmanipulering, så vi har skapat en funktion med hjälp av "Regex" för att ersätta matchande tecken med en tom sträng, vilket i grunden tar bort dem.

Alternativt kan detta uppnås genom att använda en annan funktion som granskar varje tecken i strängen och bara behåller de som inte matchar mönstret, men denna metod kan vara långsammare när det gäller stora datamängder.

## Se även
För mer information kring ämnet, ta gärna en titt på följande källor:
- [Elm Guide](https://guide.elm-lang.org/)
- [Introduction to Elm](https://elmprogramming.com/)
- [Elm Regex library](https://package.elm-lang.org/packages/elm/regex/latest/)