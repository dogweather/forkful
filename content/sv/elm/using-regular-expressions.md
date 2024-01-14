---
title:                "Elm: Användning av reguljära uttryck"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck (regular expressions) är ett kraftfullt verktyg inom programmering som möjliggör matching and manipulation av strängar. Genom att använda reguljära uttryck i dina Elm-program, kan du enkelt filtrera, ersätta och extrahera data från textsträngar. Detta gör det möjligt för dig att manipulera data på ett smidigt och effektivt sätt.

## Så här gör du

För att använda reguljära uttryck i Elm, behöver du använda modulen `Regex`. Först måste du importera den i din kod genom att lägga till följande rad i början av din fil:

``` Elm
import Regex
```

Sedan kan du använda funktionen `Regex.contains` för att matcha en del av en sträng. Här är ett exempel där vi matchar förekomsten av bokstäverna "hej" i strängen "Hej på dig!":

``` Elm
Regex.contains (Regex.regex "hej") "Hej på dig!"  -- ger True
```

Du kan också använda funktionen `Regex.replace` för att ersätta en del av en sträng med en viss text. Här är ett exempel där vi ersätter alla blanksteg i en sträng med "_":

``` Elm
Regex.replace (Regex.regex " ") (\_ -> "_") "Hej på dig!"  -- ger "Hej_på_dig!"
```

Slutligen kan du också använda funktionen `Regex.find` för att hitta en del av en sträng som matchar ett visst mönster. Här är ett exempel där vi hittar första förekomsten av en siffra i en sträng:

``` Elm
Regex.find (Regex.regex "\\d+") "Det finns 2 katter i huset"  -- ger Just (Regex.Match "2")
```

## Djupdykning

Det finns många olika mönster som du kan använda för att skapa reguljära uttryck. Här är några användbara mönster som kan hjälpa dig att göra mer avancerade matchningar:

- `.`: Matchar vilken tecken som helst.
- `\\s`: Matchar blanksteg.
- `[A-Za-z0-9]`: Matchar en alfabetisk eller numerisk karaktär.
- `\\w`: Matchar en alfanumerisk karaktär.
- `^`: Används för att matcha början av en sträng.
- `$`: Används för att matcha slutet av en sträng.

Du kan också använda speciella modifierare för att göra dina uttryck mer specifika. Några exempel på sådana modifierare är `*` (noll eller flera förekomster), `+` (en eller flera förekomster) och `?` (noll eller en förekomst).

Det finns också många andra funktioner som du kan använda med `Regex`-modulen, såsom `matches`, `findAll`, och `findSubmatches`.

## Se även

- [Regexp Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Elm Dokumentation - Regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)