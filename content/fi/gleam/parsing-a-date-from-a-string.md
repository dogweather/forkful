---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Datan parsiminen merkkijonosta tarkoittaa päivämäärätiedon erottamista merkkijonosta. Ohjelmoijat tekevät sen, jotta he voivat käsitellä päivämäärää ohjelmissaan.

## Kuinka se tehdään:
Tässä näyte Gleam-koodista, jossa parsitaan päivämäärä merkkijonosta.

```Gleam
import gleam/date
import gleam/regex

fn parse_date(date: String) -> Result(Date, Nil) {
  let Ok(regex) = regex.from_string("^([0-9]{4})-([0-9]{2})-([0-9]{2})$")
  case regex.find(date) {
    Some([_, year, month, day]) ->
      date.new(year, month, day)
    _ ->
      Error(Nil)
  }
}
```

Jos syötät `"2022-09-27"` ja kutsut parse_date funktiota, saat `Ok(#Date(year: 2022, month: 9, day: 27))`.

## Syvällisemmin:
Päivämäärän parsintaa on käytetty ohjelmoinnissa niin kauan kuin tietokoneet ovat käsitelleet päivämääriä. Vaihtoehtoisesti voitaisiin käyttää käytännön syistä valmiiksi muotoiltuja päivämääriä. Yksityiskohdista voi mainita, että Gleam-kielessä voit parsia päivämäärän sekä yhdistetyllä päiväyksellä (DST) tai ilman.

## Katso myös:
Aiheesta voit lukea lisää Gleam-ohjelmointikielestä heidän virallisilta verkkosivuiltaan: https://gleam.run
Päivämäärien käsittelyyn liittyen voit katsoa myös tämän: https://docs.gleam.run/tour/dates.html