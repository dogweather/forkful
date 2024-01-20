---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla stora bokstäver till småtbokstäver i en given textsträng. Programmerare gör detta för att underlätta textmatchning och sortering, eftersom datorer skiljer på stora och små bokstäver.

## Hur man gör:
I Gleam används funktionen `to_lower` i `string`-modulen för att konvertera till gemener. Här är ett exempel:

```Gleam
import gleam/string

fn main() -> Nil {
  let sträng = "Hej VÄRLDEN!"
  let gentemener = string.to_lower(sträng)
  assert(gentemener == "hej världen!")
}
```

Kör denna kod, och utmatningen kommer att bli:
```Gleam
"hej världen!"
```

## Fördjupning
Historiskt sett användes konvertering till gemener i början av datortiden för att spara utrymme, eftersom det inte fanns plats för både stora och små bokstäver i vissa lagringssystem. I Gleam och de flesta moderna språk, används detta för jämförelser och sortering. 

Ett alternativ till `string.to_lower` kan vara att skriva en egen funktion, men detta rekommenderas inte eftersom det är troligt att det blir fel. 

`to_lower`-funktionen i Gleam fungerar genom att gå igenom varje tecken i strängen, kolla om det är en stor bokstav, och om det är, ändra den till en liten bokstav.

## Se även
1. Gleam language doc - [Link](https://gleam.run/book/tour/basics.html)
2. Expand your Gleam skills with "Learn X in Y minutes: Gleam" - [Link](https://learnxinyminutes.com/docs/gleam)
3. For more on string manipulation, here's a [Link](https://gleam.run/stdlib/string.html) to the official String module docs.

## See Also
1. Gleam språkdokumentation - [Länk](https://gleam.run/book/tour/basics.html)
2. Bredda dina Gleam färdigheter med "Learn X in Y minutes: Gleam" - [Länk](https://learnxinyminutes.com/docs/gleam)
3. För mer om strängmanipulation, här är en [Länk](https://gleam.run/stdlib/string.html) till den officiella dokumentationen för String-modulet.