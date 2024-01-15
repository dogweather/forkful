---
title:                "Radera tecken som matchar ett mönster."
html_title:           "Haskell: Radera tecken som matchar ett mönster."
simple_title:         "Radera tecken som matchar ett mönster."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara en användbar funktion när man hanterar textdata eller vill rensa ut onödiga tecken från en sträng.

## Hur man gör det

Haskell har ett inbyggt verktyg som heter `delete` för att ta bort element från en lista baserat på ett visst villkor. I detta fall kommer vi att använda funktionen `isAlpha` för att kolla om det är ett alfabetiskt tecken och sedan ta bort det från vår sträng.

```Haskell
deleteChars :: String -> String
deleteChars str = delete `isAlpha` str
```

Om vi kör funktionen med en sträng som innehåller "Hello World!" som innehåller både bokstäver och symboler, kommer vi att få ut "HelloWorld" som ett resultat.

## Djupdykning

Haskell har en mängd olika funktioner för att hantera textdata, inklusive `delete`. Det finns också funktioner som `filter`, `map` och `fold` som kan användas för att manipulera strängar på olika sätt. Man kan också skapa egna funktioner för att uppnå önskade resultat.

En annan användbar funktion som ofta används i samband med `delete` är `takeWhile`, som låter oss ta bort tecken från en lista tills ett villkor inte längre är uppfyllt. Det finns också möjlighet att ta bort enbart delar av en sträng genom att använda indexering, till exempel `str !! index` för att få ett specifikt tecken.

## Se också

- [Haskell.org](https://www.haskell.org/) - Officiell hemsida för Haskell
- [Learn You a Haskell](http://learnyouahaskell.com/) - En interaktiv guide för att lära sig Haskell
- [CodeWars](https://www.codewars.com/?language=haskell) - Träna på att lösa problem och utöka din Haskell-kunskap