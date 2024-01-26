---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:14:28.556702-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä ja Miksi?"
Kun ohjelmoijat nappaavat nykyhetken päivämäärän, he yleensä tarvitsevat aikaleimoja, aikavälejä tai haluavat tehdä päätöksiä reaaliajassa. Nykyinen päivämäärä auttaa sovelluksia pysymään ajantasaisina ja vuorovaikutuksessa käyttäjän kanssa.

## How to:
"Näin se tehdään:"
```gleam
import gleam/utc_now.{utc_now}
import gleam/io.{println}

fn show_current_date() {
  let today = utc_now() // Hakee nykyhetken UTC-aikaan
  println(today)        // Tulostaa päivämäärän
}

pub fn main() {
  show_current_date()
}
```
Esimerkkituloste: `2023-03-14T15:09:12Z`

## Deep Dive
"Syväsukellus"
Ensimmäisen kerran päivämäärän hakeminen ohjelmallisesti tuli mahdolliseksi jo varhaisissa tietokoneissa. Gleamissa päivämäärän hallinta on yksinkertaista, mutta muitakin kirjastoja ja menetelmiä on olemassa, kuten `calendar` ja `datetime`. UTC on yleensä suositeltavampi aikavyöhyke, koska se on standardoitu eikä se muutu kesä- tai talviajan mukana.

## See Also
"Lisätietoja"
- Official Gleam language website: [Gleam Language](https://gleam.run)
