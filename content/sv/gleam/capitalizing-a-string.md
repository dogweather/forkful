---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla första bokstaven i varje ord till versal (stor bokstav), vilket ofta används för att framhäva titlar eller rubriker. Programmerare gör detta för att följa stilguider eller förbättra textens läsbarhet.

## How to:
Kapitalisera en sträng i Gleam är rakt på sak. Använd funktionen `string.capitalize` på din sträng. Här är ett enkelt exempel:

```gleam
import gleam/string

fn main() {
  let greeting = "hej världen!"
  let capitalized_greeting = string.capitalize(greeting)
  capitalized_greeting
}
```

Kört detta skulle ge oss:
```
"Hej världen!"
```

## Deep Dive
I historiskt perspektiv har string-kapitalisering funnits så länge som vi har haft behov av att behandla text i datorer. I Gleam och andra funktionella programmeringsspråk är string-kapitalisering ofta inbyggd i standardbiblioteket. 

Det finns olika sätt att kapitalisera en sträng. Förutom `string.capitalize`, som kapitaliserar endast det första ordet, kan man använda andra funktioner för att exempelvis göra alla bokstäver till versaler (uppercasing) eller gemen (lowercasing). Implementationen av `string.capitalize` i Gleam hanterar unicode korrekt, vilket är viktigt eftersom olika språk har olika regler för kapitalisering.

## See Also
- En artikel om Unicode och behandling av strängar: [https://unicode.org/reports/tr15/](https://unicode.org/reports/tr15/)
- Stilguider för programmerare: [https://google.github.io/styleguide/javaguide.html](https://google.github.io/styleguide/javaguide.html)
