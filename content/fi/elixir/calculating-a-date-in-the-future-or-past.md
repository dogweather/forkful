---
title:    "Elixir: Tulevan tai menneen päivämäärän laskeminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Moni saattaa ihmetellä, miksi haluaisimme laskea päivämäärän tulevaisuudessa tai menneisyydessä. Yksi syy tähän voi olla esimerkiksi sovelluksessa tarvittavat aikarajat tai laskut, jotka perustuvat tiettyyn päivämäärään.

## Miten

Laskeminen tiettyä päivämäärää tulevaisuudessa tai menneisyydessä on helppoa Elixir-ohjelmointikielellä. Voimme käyttää siihen `DateTime`-moduulia, jonka sisältämät funktiot helpottavat ajankäytön käsittelyä.

```elixir
iex> DateTime.add(DateTime.utc_now, weeks: 2)
#=> ~U[2021-05-17 14:58:30.265057Z]

iex> DateTime.add(DateTime.utc_now, years: -5)
#=> ~U[2016-05-03 14:58:30.265057Z]
```

Näissä esimerkeissä käytimme `DateTime.add`-funktiota lisäämään tai vähentämään aikaa annetuilla parametreilla. Voit myös tarkistaa halutun päivämäärän eroa nykyhetkestä käyttämällä `DateTime.diff`-funktiota.

```elixir
iex> DateTime.diff(~U[2022-12-25], DateTime.utc_now, :days)
#=> 596
```

Tämä palauttaa päivien määrän nykyhetkestä tulevaan joulupaivaan.

## Syvempi sukellus

Elixirin `DateTime`-moduuli sisältää monia hyödyllisiä funktioita, kuten `DateTime.truncate`, joka leikkaa aikaleimasta pois halutun ajan yksiköt. Tämä voi olla hyödyllistä esimerkiksi vertaillessa kahta päivämäärää.

```elixir
iex> DateTime.truncate(DateTime.utc_now, :hour)
#=> ~U[2021-04-28 14:00:00.000Z]
```

Voit myös muuttaa aikavyöhykettä `DateTime.change_timezone`-funktiolla.

```elixir
iex> DateTime.change_timezone(DateTime.utc_now, "Europe/Helsinki")
#=> ~U[2021-04-28 17:58:30.265057+03:00]
```

Voit tutustua kaikkiin `DateTime`-moduulin tarjoamiin toimintoihin Elixirin virallisesta dokumentaatiosta.

## Katso myös

- [Elixirin virallinen dokumentaatio DateTime-moduulista](https://hexdocs.pm/elixir/DateTime.html)
- [Elixirin Github-repositorio](https://github.com/elixir-lang/elixir)