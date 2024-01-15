---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Elixir: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat lukea tämän artikkelin, olet ehkä kiinnostunut selvittämään nykyisen päivämäärän Elixir-ohjelmointikielellä. Tämä voi olla hyödyllistä esimerkiksi päivää laskiessa tai tiettyjen aikapohjaisten toimintojen suorittamiseen ohjelmassa.

## Kuinka

```elixir
current_date = Date.utc_today()
IO.inspect(current_date)
```

Koodinpätkä hakee nykyisen päivämäärän UTC-aikavyöhykkeellä ja tulostaa sen konsoliin. Voit myös tallentaa päivämäärän muuttujaan ja käyttää sitä myöhemmin muissa ohjelman osissa.

```elixir
{year, month, day} = Date.utc_today()
```

Tämä esimerkki näyttää, kuinka voit eritellä päivämäärän osiin ja tallentaa ne omiin muuttujiin. Näin voit esimerkiksi luoda oman formaatin päivämäärälle.

## Syvempi sukellus

Elixirissä on sisäänrakennettu Date-moduuli, joka tarjoaa useita funktioita ja metodeja päivämäärien käsittelyyn. Voit esimerkiksi käyttää `Date.add/2` -funktiota lisätäksesi tai vähentääksesi tietyn ajanjakson nykyisestä päivämäärästä.

```elixir
next_week = Date.add(current_date, 7, :day)
```

Tämä koodi lisää 7 päivää nykyiseen päivämäärään ja tallentaa sen `next_week` -muuttujaan.

Voit myös käyttää `Date.compare/2` -funktiota vertaamaan kahta päivämäärää keskenään.

```elixir
{:ok, comparison} = Date.compare(next_week, current_date)
IO.puts "Next week is #{comparison} than today."
```

Tässä koodissa vertaillaan `next_week`-muuttujan ja `current_date`-muuttujan välisiä päivämääriä ja tulostetaan ero konsoliin.

## Katso myös

- [Elixirin virallinen dokumentaatio Date-moduulista](https://hexdocs.pm/elixir/Date.html)
- [Elixir School -oppimissivusto](https://elixirschool.com/fi/) tarjoaa kattavan oppaan Elixir-ohjelmointikieleen
- [Jaaen Matsdotterin blogipostaus aiheesta](https://medium.com/@jannematthias/fun-with-exirls-date-module-4fdd39b3c470), jossa hän käy läpi erilaisia päivämääräfunktioita Elixirissä.