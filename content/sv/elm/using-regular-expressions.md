---
title:                "Använda reguljära uttryck"
html_title:           "Elm: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är ett användbart verktyg för programmerare för att matcha och manipulera textsträngar. De består av ett mönster med metatecken som kan matcha olika tecken och uttryck i en sträng. Genom att använda reguljära uttryck kan programmerare effektivt söka och ersätta text, validera inmatningar och mycket mer.

## Hur man:
För att använda reguljära uttryck i Elm, måste vi först importera elm/regex biblioteket. Här är ett exempel på hur man kan matcha en sträng med ett enkelt mönster:

```Elm
import elm/regex

matcha : Regex-resultat String
matcha =
    Regex.match "Hello" "Hello, världen!"
```

Output: ```Just "Hello"```

Vi kan också använda metatecken som "." för att matcha vilket tecken som helst och "*" för att matcha det föregående uttrycket 0 eller fler gånger. Till exempel:

```Elm
import elm/regex

matcha : Regex-resultat String
matcha =
    Regex.match "h.llo*" "hellooooo"
```

Output: ```Just "hellooooo"```

## Djupdykning:
Regulära uttryck har funnits i decennier och har använts i olika programmeringsspråk, men det är fortfarande ett kraftfullt verktyg som används idag. Det finns också alternativ till reguljära uttryck, som mönstermatchning i Elm, men reguljära uttryck kan fortfarande vara användbara i vissa situationer där vi behöver mer avancerad funktionalitet.

I Elm används reguljära uttryck genom att konvertera dem från strängar till Regex typen, som kan jämföras med andra strängar med hjälp av inbyggda funktioner som "match" och "replace". Det är också möjligt att använda Regex.lowercase för att matcha strängar med olika fall.

## Se även:
[Elm Regex documentation](https://package.elm-lang.org/packages/elm/regex/latest/)