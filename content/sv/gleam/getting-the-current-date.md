---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:14:33.205121-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum innebär att få fram dagens datum på servern där din kod körs. Detta används ofta för att logga händelser, hantera tidsspecifika data eller tidstämpla användaraktiviteter.

## How to:
I Gleam använder du Standardbiblioteket för sådant som tidsberäkningar. Här är en snippet för att få det aktuella datumet:

```gleam
import gleam/calendar

pub fn main() {
  let today = calendar.local_now()
  today
}
```

Kör koden och utdatan visar något i stil med:

```
# output
calendar.DateTime(...)
```

## Deep Dive
Funktionsanropet `calendar.local_now()` i Gleam ger oss en `DateTime` struktur som representerar aktuellt datum och tid baserat på serverns lokal tid. I äldre språk eller plattformar kunde det vara mer komplicerat, men Gleam håller det enkelt och säkert.

Alternativet till `calendar.local_now()` är att använda `calendar.utc_now()`, som ger dig den aktuella tiden i UTC-format. Vilken du väljer beror på ditt behov – lokal tid eller universell tid.

När det gäller implementationen, så hanterar Gleam datumen genom att använda Erlang:s underliggande system, vilket är pålitligt och väl testat genom åren. Det är glädjande okomplicerat – något Gleam strävar efter i alla aspekter av språket.

## See Also
- Erlang's documentation on date and time: [Erlang DateTime](http://erlang.org/doc/man/calendar.html)  
- Time and date programming best practices: [Time and Date Best Practices](https://www.w3.org/TR/timezone/)
