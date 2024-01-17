---
title:                "Jämföra två datum"
html_title:           "Gleam: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ett viktigt koncept inom programmering är jämförelse av datum. Det innebär att man undersöker om ett datum är tidigare, senare eller samma som ett annat datum. Detta är viktigt för att kunna hantera och sortera datumdata på ett effektivt sätt i program.

## Så här:
Enkelt uttryckt så jämför man två datum genom att använda villkorsuttryck som if-satser, som kollar om ett datum är mindre än, större än eller lika med det andra datumet. 
```Gleam
if date1 < date2 {
  print("date1 är tidigare än date2")
} else if date1 > date2 {
  print("date1 är senare än date2")
} else {
  print("date1 och date2 är samma datum")
}
```

En annan möjlighet är att använda inbyggda funktioner för datumjämförelse, som till exempel `compare_dates()` eller `order_dates()`. Dessa funktioner returnerar en siffra som indikerar om ett datum är före, efter eller samma som det andra datumet.

```Gleam
let comparison = compare_dates(date1, date2)
if comparison == -1 {
  print("date1 är tidigare än date2")
} else if comparison == 1 {
  print("date1 är senare än date2")
} else {
  print("date1 och date2 är samma datum")
}
```

## Djupdykning:
Historiskt sett har det funnits olika sätt att implementera jämförelse av datum, men oftast baserat på hur många dagar som gått sedan ett urdatum. Med moderna programmeringsspråk som Gleam finns det inbyggda funktioner och bibliotek som gör detta enklare.

Det finns även olika format för datumrepresentation, såsom Unix-timestamps eller ISO-standarder, vilket kan påverka hur jämförelser görs.

Alternativ till Gleam för datumjämförelse inkluderar andra programmeringsspråk som Rust och Elixir.

## Se även:
- Officiell dokumentation för Gleam: https://gleam.run/
- Samhällsdiskussioner om Gleam: https://elixirforum.com/t/gleam-a-smooth-new-language-for-erlang-vm/29080
- Officiell dokumentation för andra programmeringsspråk: https://www.rust-lang.org/ och https://elixir-lang.org/