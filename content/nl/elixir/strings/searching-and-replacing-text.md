---
title:                "Tekst zoeken en vervangen"
aliases:
- /nl/elixir/searching-and-replacing-text.md
date:                  2024-01-28T22:07:17.992710-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen van tekst zijn basisvaardigheden voor programmeren; het gaat in essentie om het vinden van strings en deze omwisselen. Programmeurs doen dit constant voor zaken zoals het updaten van codebases, het verwerken van tekstgegevens of gewoon simpele bewerkingstaken.

## Hoe te:

In Elixir kun je de `String` module gebruiken voor snelle zoek-en-vervang-operaties. Hier is hoe je dat doet:

```elixir
originele_tekst = "I heart Elixir!"

# Simpel vervangen
vervangen_tekst = String.replace(originele_tekst, "heart", "❤️")
IO.puts vervangen_tekst  # Uitvoer: I ❤️ Elixir!

# Globaal vervangen met een patroon
vervangen_tekst_globaal = String.replace(originele_tekst, ~r/eart|Eli/, "❤️", global: true)
IO.puts vervangen_tekst_globaal  # Uitvoer: I ❤️ ❤️xir!

# Hoofdletterongevoelig vervangen
hoofdletterongevoelig_vervangen = String.replace(originele_tekst, "ELIXIR", "❤️", global: true, case_insensitive: true)
IO.puts hoofdletterongevoelig_vervangen  # Uitvoer: I heart ❤️!
```

## Diepere Duik

Zoeken en vervangen van tekst bestaat al sinds de dageraad van de informatica; denk aan 'zoek en vervang' in een Word-document, maar dan voor code. In Elixir draait het allemaal om patroonmatching en effectief werken met strings.

De functie `String.replace/4` maakt gebruik van de patroonmatchingmogelijkheden van Elixir, waardoor je niet alleen statische strings maar ook regex-patronen kunt matchen, wat aanzienlijke flexibiliteit biedt. Achter de schermen gebruikt Elixir de krachtige stringafhandeling van Erlang, die robuust en efficiënt is voor tekstverwerkingstaken.

Alternatieven voor de ingebouwde `String` module zijn het schrijven van je eigen functies voor complexere gevallen of het gebruik van externe bibliotheken die stringafhandeling op verschillende manieren omvatten. Echter, voor de meeste gebruikssituaties zullen de ingebouwde functies het werk klaren zonder extra afhankelijkheden toe te voegen.

Als een onveranderlijke taal, onthoud dat elke vervangfunctie een nieuwe string retourneert - de originele blijft ongewijzigd. Dit is anders dan in sommige andere talen waar je misschien de string ter plaatse wijzigt.

## Zie Ook

- De docs van Elixir's `String` module: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex in Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Leer meer over patroonmatching in Elixir: [https://elixir-lang.org/getting-started/pattern-matching.html](https://elixir-lang.org/getting-started/pattern-matching.html)
