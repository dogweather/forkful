---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Elixir: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Laskenta tulevaisuudessa tai menneisyydessä olevaan päivämäärään tarkoittaa tietyssä ajankohtassa tapahtuvan siirron laskemista ajan yli. Ohjelmoijat suorittavat tämän toimenpiteen erilaisten ajankohtaisten tarpeiden, kuten vanhenevien tunnusten tai eräpäivien määrittämiseksi.

## Miten:

Elixirissä voit laskea tulevat tai menneet päivämäärät `DateTime.add/3` -funktiolla.

```Elixir
start_date = DateTime.utc_now()
IO.inspect(start_date)

# +3 days
new_date = DateTime.add(start_date, 3 * 24 * 60 * 60, :second)
IO.inspect(new_date)

# -3 days
new_date = DateTime.add(start_date, -3 * 24 * 60 * 60, :second)
IO.inspect(new_date)
```
Havainnollistavan esimerkin tulostus olisi seuraava:
```Elixir
~U[2022-07-07T18:20:30Z] 
~U[2022-07-10T18:20:30Z]
~U[2022-07-04T18:20:30Z]
```

## Syvempi sukellus:

Historiaa: Elixirin DateTime-moduuli on ollut käytössä sitten version 1.3. Monia muita käyttäjäystävällisiä päivämäärä- ja aika-toimintoja on lisätty alusta lähtien.

Vaihtoehdot: Voit käyttää myös `Time.add/3` ja `Date.add/2` lähtökohtana, riippuen siitä, haluatko muuttaa vain ajan tai päivämäärän.

Toteutuksen yksityiskohdat: DateTime.add käyttää Erlangin :calendar-moduulia aikaa ja päivämääriä koskevien yksityiskohtien käsittelyyn. Siinä on kaikki tarvittavat työkalut aika-arvojen lisäämiseen tai vähentämiseen.

## Katso myös:

- [Elixir School: Dates and Times](https://elixirschool.com/en/lessons/basics/date_time/)
- [HexDocs: DateTime Module](https://hexdocs.pm/elixir/DateTime.html)
- [HexDocs: Time add Function](https://hexdocs.pm/elixir/Time.html#add/3)
- [HexDocs: Date add Function](https://hexdocs.pm/elixir/Date.html#add/2)