---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:30:56.285688-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Räkna ut ett framtida eller tidigare datum handlar om att hitta en specifik tidpunkt före eller efter en given datum. Programmerare gör detta för att hantera bokningar, händelsepåminnelser, eller tidslinjer i olika applikationer.

## Så här gör du:
I Gleam kan du hantera datum och tid med hjälp av externa bibliotek, till exempel `chrono`. Här är ett exempel:

```gleam
import gleam/calendar
import gleam/chrono.{DateTime, Duration}

pub fn main() {
  let now = DateTime.now_utc()
  let duration = Duration.from_days(5)
  let future_date = DateTime.add_duration(now, duration)

  future_date
}
```

Och om vi kör detta får vi en output som liknar följande med dagens datum plus fem dagar:

```plaintext
2023-04-05T14:30:00Z
```

## Fördjupning
Historiskt sett hade programmerare inte bibliotek för datumhantering och var tvungna att hantera komplexiteten med tidszoner och skottår på egen hand. Nu för tiden är `chrono` och liknande bibliotek standard för datumoperationer. Det är viktigt att välja rätt bibliotek som hanterar komplexiteten med tid och datum på ett korrekt sätt, t.ex. tidszoner och skottsekunder. Alternativer till `chrono` kan till exempel vara `time` eller `date` biblioteket som också ger funktioner för att hantera tid och datum.