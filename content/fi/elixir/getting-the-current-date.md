---
title:                "Elixir: Päivämäärän haku"
simple_title:         "Päivämäärän haku"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa lukemaan blogipostausta Elixir-ohjelmoinnista! Tässä artikkelissa käsittelemme sitä, miksi olisi hyödyllistä hakea nykyinen päivämäärä ohjelmointikielellä.

Tärkein syy tähän on, että useissa ohjelmointiprojekteissa on tarpeen saada tietoa nykyisestä päivästä. Tämä voi liittyä esimerkiksi tiettyihin aikapohjaisiin suorituksiin tai raporttien luomiseen.

## Miten

Elixir-ohjelmointikielellä nykyisen päivämäärän saaminen on helppoa. Käytämme tähän tarkoitukseen `Date.utc_today()` -funktiota, joka palauttaa nykyisen päivän tiedot UTC-aikavyöhykkeellä.

```Elixir
date = Date.utc_today()
IO.inspect date

# Tulostus:
# ~D[2021-01-01]
```

Voit myös tallentaa päivämäärän muuttujaan ja käyttää sitä muissa toiminnoissa. Voit esimerkiksi saada päivän numeron ja viikonpäivän nimen käyttämällä `Date.day` ja `Date.day_of_week` -funktioita.

```Elixir
day = Date.day(date)
day_name = Date.day_of_week(date)

IO.puts "Tänään on #{day_name}, #{day}"
# Tulostus: Tänään on perjantai, 1
```

## Syvempi sukellus

On tärkeää huomata, että `Date.utc_today()` -funktio palauttaa päivämäärän UTC-aikavyöhykkeellä. Jos haluat saada päivämäärän paikallisessa aikavyöhykkeessä, voit käyttää `Date.today()` -funktiota ja antaa parametrina oman aikavyöhykkeesi.

Lisäksi, jos haluat käsitellä tietoa nykyajassa, voit käyttää `DateTime.utc_now()` -funktiota. Tämä palauttaa tietueen, joka sisältää nykyisen päivämäärän ja kellonajan UTC-aikavyöhykkeellä.

## Katso myös

- [Elixirin virallinen dokumentaatio päivämäärien käsittelystä](https://hexdocs.pm/elixir/DateTime.html)
- [Aikavyöhykkeet Elixirissä](https://elixirschool.com/blog/time-and-date-in-elixir/)
- [Elixirin sisäänrakennetut `Date` ja `DateTime` -moduulit](https://elixir-lang.org/docs/stable/elixir/Date.html)