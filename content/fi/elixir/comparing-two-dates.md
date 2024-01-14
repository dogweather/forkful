---
title:    "Elixir: Kahden päivämäärän vertailu"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi Vertailla Kahta Päivämäärää?

Vertaileminen on tärkeä osa ohjelmointia, ja se auttaa meitä tekemään tärkeitä päätöksiä koodissamme. Vertailemalla kahta päivämäärää voimme esimerkiksi selvittää, onko tapahtuma tapahtunut ennen vai jälkeen toista tapahtumaa. Tämä voi olla hyödyllistä esimerkiksi ajastettujen toimintojen toteuttamisessa. Elixirin ansiosta vertaileminen kahden päivämäärän välillä on erittäin helppoa.

## Miten Vertailla Kahden Päivämäärän Välillä?

Vertailun tekeminen kahden päivämäärän välillä Elixirissä onnistuu helposti käyttämällä Date-kirjastoa. Se sisältää paljon hyödyllisiä funktioita, joilla voimme vertailla päivämääriä ja saada tarvittavaa tietoa. Esimerkiksi voimme käyttää funktiota `Date.compare/2` vertailemaan kahta päivämäärää ja saamaan tulokseksi joko 1, 0 tai -1 riippuen siitä, kumpi päivämäärä on aikaisempi. Alla olevassa koodiesimerkissä vertailemme kahta eri päivämäärää ja tulostamme tulokset.

```Elixir
date1 = ~D[2021-01-01]
date2 = Date.today()

IO.inspect Date.compare(date1, date2)  # Output: -1
IO.inspect Date.compare(date2, date1)  # Output: 1
IO.inspect Date.compare(date2, date2)  # Output: 0
```

## Syvemmälle Päivämäärän Vertailuun

Vertailemalla kahden päivämäärän välillä Elixirin Date-kirjaston avulla voimme saada myös tarkempaa tietoa päivämääristä. Voimme esimerkiksi tarkistaa, onko kaksi päivämäärää samat käyttämällä funktiota `Date.equal?/2`. Voimme myös tarkistaa, kuinka monta päivää kahden päivämäärän välillä on käyttämällä funktiota `Date.diff/2`. Tässä vielä lisäesimerkki, jossa tulostamme kahden päivämäärän välisen eron päivinä.

```Elixir
date1 = ~D[2021-01-01]
date2 = Date.today()

IO.puts Date.diff(date1, date2, :days)  # Output: 223
```

## Katso Myös

- [Elixirin dokumentaatio Date-kirjastosta](https://hexdocs.pm/elixir/Date.html)
- [Vertailufunktioiden opas Date-kirjastossa](https://elixirschool.com/en/lessons/specifics/compare/)
- [Elixirin perusteet - Opas suomeksi](https://elixirko.github.io/alku/)