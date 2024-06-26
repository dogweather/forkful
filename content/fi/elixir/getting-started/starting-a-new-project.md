---
date: 2024-01-20 18:03:30.608583-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) Elixiriss\xE4 projekti k\xE4ynnistet\xE4\
  \xE4n k\xE4ytt\xE4m\xE4ll\xE4 Mix-ty\xF6kalua. Mix on Elixiriin sis\xE4\xE4nrakennettu\
  \ ty\xF6kalu, joka hallinnoi\u2026"
lastmod: '2024-04-05T21:53:57.783892-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) Elixiriss\xE4 projekti k\xE4ynnistet\xE4\xE4n k\xE4\
  ytt\xE4m\xE4ll\xE4 Mix-ty\xF6kalua."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: (Kuinka tehdään:)
Elixirissä projekti käynnistetään käyttämällä Mix-työkalua. Mix on Elixiriin sisäänrakennettu työkalu, joka hallinnoi riippuvuuksia ja tehtäviä. Tässä on miten tehdään:

```Elixir
# Asenna Elixir, jos sitä ei ole jo
mix new esimerkki_projekti
cd esimerkki_projekti
# Tarkista, että projekti toimii
mix test
```

Tämän pitäisi antaa sinulle tuloste, joka ilmoittaa, että testit ohittuivat:

```
Compiling 1 file (.ex)
Generated esimerkki_projekti app
..

Finished in 0.02 seconds
2 tests, 0 failures

Randomized with seed 54321
```

## Deep Dive (Sukellus syvyyksiin):
Elixir julkaistiin vuonna 2011, ja se on suunniteltu erityisesti skaalautuvuuden ja ylläpidettävyyden tarpeisiin. Se hyödyntää Erlang VM:ää (BEAM), joka on tunnettu rinnakkaisuudestaan ja vikasietoisuudestaan.

Vaihtoehtoisia työkaluja projektin aloitukseen Elixirissä ovat muun muassa Phoenix (web-kehikko) ja Nerves (ohjelmisto järjestettyyn laitteistokehitykseen). Perustavanlaatuista valmistelua ei tarvita, koska Mix auttaa kaiken tarvittavan pystyttämisessä, mukaan lukien testirungon ja moduulirakenteen.

## See Also (Lisätietoja):
- Elixirin virallinen sivusto: https://elixir-lang.org/
- Mix-työkalun dokumentaatio: https://hexdocs.pm/mix/Mix.html
- Phoenix-kehikon opas: https://www.phoenixframework.org/
- Nerves-projektin verkkosivu: https://www.nerves-project.org/
- Elixirin oppaat ja tutoriaalit Alchemist Campissa: https://alchemist.camp/lessons
