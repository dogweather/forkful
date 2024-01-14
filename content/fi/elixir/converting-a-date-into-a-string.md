---
title:                "Elixir: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Usein tämä on tarpeen muun muassa tietokannan kanssa työskennellessä tai kun sinun täytyy näyttää päivämäärä käyttäjälle selkeässä muodossa.

## Kuinka?

```Elixir
Date.to_string(Date.utc_today())
```

Tämä koodi muuttaa tänään olevan päivämäärän UTC-ajassa merkkijonoksi. 

```Elixir
"UTC Today: 2019-11-10"
```

Voit myös halutessasi muuttaa päivämäärän haluamaasi muotoon. Esimerkiksi:

```Elixir
Date.to_string(Date.utc_today(), "{YYYY}-{MM}-{DD}") 
```

Palauttaa päivämäärän tänään muodossa "2019-11-10".

## Syväsukellus

Merkkijonoksi muuttamisen toiminta perustuu pohjimmiltaan Elixirin Date-moduulin toimintaan. Date-moduuli tarjoaa useita funktioita, joilla voi muuttaa päivämäärän eri muodoiksi, esimerkiksi Date.to_iso8601 ja Date.to_ical.

Voit myös käyttää Elixirin hienoa hakukyselykirjastoa, kuten Timexiä, joka tarjoaa lisämahdollisuuksia päivämäärän käsittelyyn.

## Katso myös

- [Elixirin virallinen dokumentaatio Date-moduulista](https://elixir-lang.org/docs/master/elixir/Date.html)
- [Timex-kirjasto](https://github.com/bitwalker/timex)
- [GenDate-kirjasto](https://github.com/securomessage/gendate)