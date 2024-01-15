---
title:                "Kahden päivämäärän vertailu"
html_title:           "Elixir: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Miksi edes vaivautua vertailemaan kahta päivämäärää? Syitä voi olla monia, mutta yleisimpiä ovat esimerkiksi tilanteet, joissa tarvitaan tarkkaa tietoa ajankohdista tai ajallinen järjestys on tärkeää.

## Miten

Vertaileminen kahden päivämäärän välillä ei ole vaikeaa Elixirilla. Käytännössä se tapahtuu käyttämällä `Date.compare/2`-funktiota ja antamalla sille kaksi päivämäärämuodossa olevaa arvoa, esimerkiksi:

```Elixir
Date.compare({2021, 4, 20}, {2021, 4, 21})
```

Tämän komennon tulos on `-1`, mikä tarkoittaa, että ensimmäinen päivämäärä on pienempi kuin toinen.

Voit myös vertailla päivämäärän osia erikseen käyttämällä `Date.after?/2`- tai `Date.before?/2`-funktioita, esimerkiksi:

```Elixir
Date.before?({2021, 4, 20}, {2021, 4, 21})
```

Tämä palauttaa `true` tässä tapauksessa, sillä ensimmäinen päivämäärä on pienempi kuin toinen.

## Syvällisempi tarkastelu

Elixirilla päivämäärien vertailemiseen on monia eri vaihtoehtoja, kuten tarkistaa ovatko ne samat (`Date.same?/2`), laskea niiden ero päivinä (`Date.diff/2`) tai jopa muuttaa ne eri aikavyöhykkeille (`Date.shift_zone/2`).

On myös mahdollista tehdä vertailuja käyttämällä `DateTime`-tyyppiä, joka sisältää myös kellonajan tiedot.

## Katso myös

- [Elixir Date -dokumentaatio](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateTime -dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir Date Calculator -sovellus](https://github.com/filmil/elixir-date-calculator)