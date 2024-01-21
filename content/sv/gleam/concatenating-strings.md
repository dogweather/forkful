---
title:                "Sammanslagning av strängar"
date:                  2024-01-20T17:34:35.580285-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammanslagning av strängar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konkatenering av strängar innebär att slå ihop textstycken så de bildar en längre sträng. Programmerare gör det för att enkelt kombinera textdata, exempelvis för att bygga meddelanden eller skapa dynamiska utskrifter.

## Så gör man:
I Gleam kan du använda `++` för att konkatenera strängar. Kika på koden och resultatet nedan.

```gleam
fn main() {
  let hälsning = "Hej, "
  let namn = "Anders!"
  let komplettHälsning = hälsning ++ namn
  komplettHälsning
}

// Output: "Hej, Anders!"
```

## Djupdykning
Historiskt sett har strängkonkatenering varit en grundläggande del av många programmeringsspråk för att hantera textdata. Det finns alternativ, som att använda formatsträngar eller bygga en lista med strängdelar och sedan foga ihop dem. I Gleam genomförs konkatenering effektivt, men vid stora mängder data kan alternativa metoder som att använda `String.concat` eller `String.builder` vara att föredra för att optimera prestanda.

## Se även
- Gleams officiella dokumentation om strängar: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)