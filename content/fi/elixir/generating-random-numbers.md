---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:11.217951-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Random-lukujen generointi on prosessi, jonka avulla luodaan ennustamattomia numeroita. Ohjelmoijat käyttävät näitä muun muassa peleissä, simulaatioissa ja turvallisuuden parantamisessa.

## How to:
Generoidaan satunnaisluku Elixirissä:

```elixir
:rand.uniform(10)
```
Tämä tuottaa satunnaisen kokonaisluvun välillä 1 ja 10.

Sarja satunnaislukuja:

```elixir
Enum.map(1..5, fn _ -> :rand.uniform(100) end)
```
Tämä luo listan viidestä satunnaisluvusta, jokainen 1:n ja 100:n väliltä.

## Deep Dive
Elixir käyttää Erlangin `:rand`-moduulia satunnaislukujen generoimiseen. Tämä moduuli tarjoaa erilaisia algoritmeja satunnaislukujen generoimiin, joiden perustana on laajalti käytetty ja tutkittu Mersenne Twister -algoritmi.

Vaihtoehtoisesti voit käyttää Hex-paketista löytyvää `exsplus`-kirjastoa, joka perustuu XORSHIFT PLUS -algoritmiin. Molemmilla on omat etunsa, mutta `:rand` on yleensä riittävä useimpiin tarpeisiin.

Satunnaislukujen generoinnin toteutuksessa on tärkeää huomioida entropian lähde. Turvallisuuskriittisissä sovelluksissa, kuten kryptografiassa, on käytettävä todella satunnaisia lähteitä, ei pelkästi pseudo-satunnaislukuja.

## See Also
Lisätietoja ja syvempää ymmärrystä varten, katso:
- Erlangin `:rand`-moduuli: https://erlang.org/doc/man/rand.html
- EXSplus-kirjasto: https://hex.pm/packages/exsplus
