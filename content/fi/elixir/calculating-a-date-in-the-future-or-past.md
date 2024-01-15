---
title:                "Tulevan tai menneen päivän laskeminen tietokoneohjelmoinnilla"
html_title:           "Elixir: Tulevan tai menneen päivän laskeminen tietokoneohjelmoinnilla"
simple_title:         "Tulevan tai menneen päivän laskeminen tietokoneohjelmoinnilla"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa ohjelman täytyy laskea tuleva tai menneellä ajanhetkellä. Se voi olla esimerkiksi tapahtumien ajanvarausta varten tai laskemassa eräpäivää laskuja varten. Tämän artikkelin avulla opit, kuinka voit tehdä tämän Elixirilla.

## Kuinka

Voit laskea tulevan tai menneen päivämäärän Elixirilla käyttämällä `DateTime`-moduulia. Alla on yksinkertainen esimerkki, jossa laskemme seuraavan päivän päivämäärän käyttämällä `DateTime.shift_naive` -funktiota:

```Elixir
date = DateTime.shift_naive(DateTime.utc_now(), days: 1)
IO.inspect(date)
```

Tämän koodin tulosteena pitäisi olla seuraavan päivän päivämäärä ISO 8601 -muodossa (esimerkiksi "2021-10-10T00:00:00.000000Z").

Jos haluat laskea menneen päivän päivämäärän, voit käyttää `DateTime.shift_naive` -funktiota samalla tavalla, mutta negatiivisella luvulla päivissä. Voit myös laskettavan päivän lisäksi määrittää muita aikayksiköitä, kuten tunteja, minuutteja tai sekunteja.

```Elixir
date = DateTime.shift_naive(DateTime.utc_now(), days: -7, hours: -3)
IO.inspect(date)
```

Tässä esimerkissä laskemme 7 päivää ja 3 tuntia takaisin. 

## Syvällisempi perehtyminen

`DateTime`-moduulin lisäksi Elixirilla on myös `Calendar`-moduuli, joka tarjoaa lisää toimintoja päivämäärän käsittelyyn. Voit esimerkiksi tarkistaa, onko tietty päivämäärä viikonloppu, käyttäen `Calendar.weekdays`-funktiota:

```Elixir
weekend_day = DateTime.from_date({2021, 10, 9})
Calendar.weekday(weekend_day) # :saturday
Calendar.weekend?(weekend_day) # true
```

Voit myös suorittaa päivämäärälaskelmia käyttäen `Calendar`-moduulin funktioita, kuten `Calendar.add/3` tai `Calendar.diff/2`. Näillä funktioilla voit lisätä tai vähentää aikayksiköitä tietystä päivämäärästä.

Lisätietoja `DateTime`- ja `Calendar`-moduuleista voit löytää [Elixirin virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/DateTime.html) .

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
- [Video opetusohjelma: Kuinka laskea päivämääriä Elixirilla](https://www.youtube.com/watch?v=dQw4w9WgXcQ)