---
title:                "Haskell: Utvinna substrängar"
simple_title:         "Utvinna substrängar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att utvinna substrängar från strängar är en vanlig uppgift inom programmering och kan vara till stor nytta i många olika situationer. Det kan till exempel användas för att söka efter specifika mönster eller ord i en text, manipulera data eller utföra byte mellan olika format. Genom att kunna extrahera substrängar kan du utöka funktionaliteten i dina program och göra dem mer effektiva.

## Hur man gör

För att utvinna en substräng i Haskell använder man funktionen "take" som tar två argument - ett tal som representerar antalet tecken som ska tas från början av strängen och själva strängen. Detta är ett enkelt sätt att få en del av en sträng.

```Haskell
take 5 "Hej världen"
```
Output: "Hej v"

För att ta bort en del av strängen från början kan man använda funktionen "drop" som tar ett tal som motsvarar antalet tecken som ska tas bort från början och strängen som ska modifieras.

```Haskell
drop 5 "Hej världen"
```
Output: "världen"

Det finns också en funktion som kombinerar "take" och "drop" som heter "takeWhile" och tar även ett villkor som avgör när substrängen ska avslutas.

```Haskell
takeWhile (/= ' ') "Hej världen"
```
Output: "Hej"

## Djupdykning

I Haskell finns det fler avancerade sätt att utvinna substrängar, såsom användning av reguljära uttryck eller att göra det rekursivt. Det är också möjligt att använda funktioner som "take" och "drop" på listor istället för strängar. Det är viktigt att ha en god förståelse för hur dessa funktioner fungerar för att kunna använda dem på ett effektivt sätt.

## Se även

- Dokumentation för "take" och "drop" funktionerna i Haskell: https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:take
- Hur man gör rekursiva substrings i Haskell: https://cs.stackexchange.com/questions/6102/computing-recursive-substrings-in-haskell
- Användning av reguljära uttryck i Haskell: https://www.mathed.nyu.edu/student_resources/web_workshops/regular-expressions/