---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:36:41.177153-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Muuttaminen päivämäärästä merkkijonoksi tarkoittaa päivämäärätiedon esittämistä selkeässä tekstiformaatissa. Ohjelmoijat tekevät tämän, jotta päivämäärä on helposti luettavissa ja yhdenmukainen eri järjestelmien välillä.

## How to:
```gleam
import gleam/calendar.{Date}
import gleam/string

fn date_to_string(date: Date) -> String {
  date
  |> calendar.to_iso8601
  |> string.from_result_with_default(default: "Invalid date")
}

pub fn main() {
  let date = Date(year: 2023, month: 4, day: 5)
  date_to_string(date)
  |> io.println
}
```

Sample output:
```
"2023-04-05"
```

## Deep Dive
Muuntaminen päivämäärästä merkkijonoksi juontaa juurensa tarpeesta dokumentoida ja siirtää aikatietoja eri järjestelmissä. ISO 8601 -standardi on kansainvälisesti hyväksytty tapa esittää päivämäärät selkeästi ja se on usein käytössä ohjelmistoissa. Gleamissa moduuli `calendar` tarjoaa toimintoja tässä standardissa pysymiseen. Valinta merkkijonon esitysmuodon välillä voi riippua kontekstista ja tarpeesta ajan esittämiseen: esimerkiksi käyttöliittymissä voi olla syytä käyttää toista formaattia kuin tietokantatallennuksessa.

## See Also
- Gleam calendar module documentation: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- ISO 8601 Date and Time Formats: https://www.iso.org/iso-8601-date-and-time-format.html
- String formatting in Gleam: https://hexdocs.pm/gleam_stdlib/gleam/string/
