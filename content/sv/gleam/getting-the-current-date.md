---
title:                "Att få dagens datum"
html_title:           "Gleam: Att få dagens datum"
simple_title:         "Att få dagens datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få dagens datum är ganska enkelt i Gleam, det är bara en fråga om att skriva några rader kod. Programmers vanligtvis använder denna funktion för att spåra tidsstämplar eller för att visa dagens datum i en användarvänlig format på en applikation.

## Hur gör man:
```
import Gleam.Date

let today = Date.now()

println(today)
// Output: 2021-05-06
```

## Djupdykning:
Att få det nuvarande datumet är en grundläggande funktion i alla programmeringsspråk, eftersom det är en viktig del av att hantera tidsbaserade operationer. I äldre språk som C och C++, lägger utvecklare vanligtvis in alla datum som en hel tidsstämpel i millisekunder, men i moderna språk som Gleam finns det inbyggda funktioner för att enkelt hämta dagens datum.

Det finns flera alternativ för att få det nuvarande datumet i Gleam, till exempel genom att använda paket som 'gleam_time' eller genom att använda externa bibliotek som 'Lixir Date'. Men den inbyggda Date-modulen är det enklaste och mest effektiva sättet att göra det.

Implementeringen av Gleam Date module baseras på Erlangs Calendar module, vilket gör det möjligt att få det nuvarande datumet i en rad olika format.

## Se även:
- Gleam Date Module Documentation: https://gleam.run/modules/Date.html
- Gleam Time Package: https://github.com/kent375/gleam_time
- Lixir Date Library: https://github.com/hexpm/libdate