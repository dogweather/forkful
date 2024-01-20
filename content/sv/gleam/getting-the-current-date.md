---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att få det aktuella datumet innebär hämta dagens datum från systemet. Detta används ofta för att märka uppgifter eller händelser med tidsstämpel, och göra tidsberoende beräkningar.

## Hur man gör:

Här är kodexempel om hur man får aktuellt datum i Gleam:

```gleam
import gleam/datetime.{Date, calendar, day, month, year, utc_now}

fn main() -> Nil {
  let current_date = utc_now()
  |> calendar
  |> to_string
  print(current_date)
}
```

Detta skickar ut något liknande: "2022-02-05 14:53:52"

## Fördjupning

Redan långt före Gleam användes liknande funktioner för att få tillgång till det aktuella datumet och tiden i många programmeringsspråk. Detta inkluderar lågnivåspråk som C och högnivåspråk som Python. Gleam har imponerade av dessa system och har implementerat lättillgängliga funktioner för att extrahera datum- och tidsinformation.

Även om Gleam funktionen `utc_now()` är det enklaste sättet att få det aktuella datumet, finns det alternativ. Till exempel kan du använda funktionen `os::system_time` för att få tiden i millisekunder sen systemstart, och sedan konvertera detta till ett datum och en tid.

I Gleam `utc_now()` funktion används Elixir :calendar.utc_now för att få UTC-datum och tid. Denna funktion ger oss en tupel och för att få en strängversion av datumet används `calendar` funktionen.

## Se även

1. Gleams officiella dokumentation om datum och tid (http://gleam.run/documentation/)

2. Elixir :calendar.utf_now funktion (https://hexdocs.pm/elixir/Calendar.html#module-utc-now)