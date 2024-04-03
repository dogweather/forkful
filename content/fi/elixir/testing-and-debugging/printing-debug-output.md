---
date: 2024-01-20 17:52:20.220810-07:00
description: "How to: (Kuinka tehd\xE4:) K\xE4ytet\xE4\xE4n `IO.puts` tai `IO.inspect`\
  \ n\xE4ytt\xE4m\xE4\xE4n tietoja konsolissa."
lastmod: '2024-03-13T22:44:56.229542-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:)\n\nK\xE4ytet\xE4\xE4n `IO.puts` tai `IO.inspect` n\xE4\
  ytt\xE4m\xE4\xE4n tietoja konsolissa."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to:
(Kuinka tehdä:)

Käytetään `IO.puts` tai `IO.inspect` näyttämään tietoja konsolissa.

```elixir
# Yksinkertainen tekstiviesti
IO.puts("Moikka maailma!")

# Muuttujan arvon tulostus
moi = "Hei taas!"
IO.inspect(moi)

# Arvon tulostus, mutta arvo palautetaan myös
IO.inspect(moi, label: "Arvon tarkastus")
```

Output:
```
Moikka maailma!
"Hei taas!"
Arvon tarkastus: "Hei taas!"
```

## Deep Dive:
(Sukellus syvemmälle:)

Alun perin, kuten monissa ohjelmointikielissä, debug-tulostus oli yksinkertainen väline ohjelman tilan tarkasteluun. Elixirissä `IO.inspect` on mennyt askelen pidemmälle: se palauttaa arvon, joten sitä voi käyttää ketjutetusti.

Jos `IO.inspect` ei riitä, voi kääntyä Erlangin :observer-moduulin tai Elixirin `:debugger`-moduulin puoleen, jotka tarjoavat visuaalisempia työkaluja.

Kehittyneempiin tarpeisiin Elixir tarjoaa `Logger`-moduulin, joka tukee eri lokitasoja ja on konfiguroitavissa.

## See Also:
(Lisätietoja:)

- Elixirin virallinen dokumentaatio `IO`: https://hexdocs.pm/elixir/IO.html
- `Logger`-moduulin dokumentaatio: https://hexdocs.pm/logger/Logger.html
- Erlangin :observer-moduuli: http://erlang.org/doc/apps/observer/observer_ug.html
- Elixirin `:debugger`: https://hexdocs.pm/elixir/master/Debugging.html
