---
title:                "Haskell: Använda reguljära uttryck"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck, även kända som regex, är ett kraftfullt verktyg inom programmering för att söka, matcha och manipulera textsträngar. Genom att lära sig hur man använder regex kan du öka din programmeringskunskap och effektivitet.

## Hur man gör

För att använda reguljära uttryck i Haskell, måste du importera modulen `Text.Regex.Posix`. Sedan kan du använda funktioner som `matchRegex` för att matcha strängar mot ditt reguljära uttryck. Se nedan för ett exempel:

```Haskell
import Text.Regex.Posix

-- Skapa ett reguljärt uttryck för att hitta alla mailadresser
pattern = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"

-- Skapa en sträng att matcha mot
text = "Hej! Mitt mail är john_doe@example.com och jag är intresserad av Haskell."

-- Använd matchRegex för att hitta alla matchningar
matches = matchRegex (makeRegex pattern) text

-- Skriv ut resultaten
print matches
```

Output:

`Just ["john_doe@example.com"]`

## Djupdykning

Reguljära uttryck kan vara svåra att förstå och skriva, men det finns många resurser där ute som kan hjälpa dig. En bra resurs är "Learn Regex the Hard Way" (https://regex.learncodethehardway.org/) som lär dig allt från grunderna till mer avancerade uttryck. Det är också viktigt att testa dina uttryck i ett regex-testverktyg för att se hur de matchar mot olika strängar.

Det finns också många olika funktioner och modifierare som kan användas med reguljära uttryck, såsom att göra matcher icke-greedy (till exempel `*?`) eller att ersätta text med `subRegex` funktionen. Utforska dessa möjligheter för att få en bättre förståelse för reguljära uttryck.

## Se även

- "Learn Regex the Hard Way": https://regex.learncodethehardway.org/
- Regex-testverktyg: https://regexr.com/
- Officiell dokumentation för `Text.Regex.Posix`: https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html