---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:20.220810-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
(Mitä & Miksi?)

Debug-tulostus auttaa ymmärtämään, mitä ohjelmassasi tapahtuu. Ohjelmoijat käyttävät sitä virheiden paikallistamiseen ja oman koodin käytöksen selvittämiseen.

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