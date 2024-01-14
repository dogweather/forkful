---
title:                "Elixir: Sattumanvaraisten numeroiden luominen"
simple_title:         "Sattumanvaraisten numeroiden luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Elixir-ohjelmointikieli tarjoaa laajan joukon työkaluja monenlaisten sovellusten kehittämiseen, mukaan lukien satunnaislukujen luominen. Generoidut satunnaisluvut voivat osoittautua erittäin hyödyllisiksi pelien ja salausmenetelmien tarkastelussa. Mikä vielä parempaa, Elixir tarjoaa helpon tavan luoda näitä satunnaislukuja.

## Miten tehdä se

Käyttämällä Elixirin rand-funktiota, voimme helposti luoda satunnaisia kokonaislukuja haluamassamme välillä. Seuraavassa esimerkissä luodaan kymmenen satunnaislukua välillä 1-100 ja tulostetaan ne näytölle:

```elixir
numbers = for _ <- 1..10, do: :rand.uniform(1..100)
IO.inspect numbers
```

Output:

```
[5, 87, 49, 36, 16, 92, 77, 57, 10, 31]
```

Voimme myös käyttää Elixirin :rand-algoritmeja luomaan erilaisia tyyppejä, kuten esimerkiksi binäärireittejä tai -merkkijonoja. Alla esimerkki, jossa luodaan 10 satunnaista merkkijonoa, joissa jokaisessa on 10 merkkiä:

```elixir
strings = for _ <- 1..10, do: :rand.uniform(1..100) |> Integer.to_string
IO.inspect strings
```

Output:

```
["80", "28", "49", "31", "71", "99", "86", "40", "91", "38"]
```

## Syvemmälle

Jos haluamme saada enemmän tietoa Elixirin satunnaislukujen luomisesta, voimme tutkia Elixirin rand-moduulia. Täältä löydämme lisätietoja erilaisista satunnaislukujen generaattoreista, kuten :rand.uniform(), :rand.binomial() ja :rand.poisson(). Lisäksi voimme tarkastella Elixirin satunnaisluku-algoritmeja, kuten Marsaglia ja Park-Miller, ja selvittää, miten ne toimivat.

## Katso myös

- [Elixirin virallinen dokumentaatio rand-moduulista](https://hexdocs.pm/elixir/1.12/Random.html)
- [Elixirin satunnaislukujen generoiminen YouTube-videolla](https://www.youtube.com/watch?v=bO1tmCi_uPU) (suomenkielinen tekstitys saatavilla)