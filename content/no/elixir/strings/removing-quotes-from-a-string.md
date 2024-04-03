---
date: 2024-01-26 03:38:39.543946-07:00
description: "\xC5 strippe anf\xF8rselstegn fra en streng inneb\xE6rer \xE5 fjerne\
  \ de ekstra innpakningene for \xE5 f\xE5 tak i den rene teksten inni. Programmerere\
  \ gj\xF8r dette for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.431593-06:00'
model: gpt-4-0125-preview
summary: "\xC5 strippe anf\xF8rselstegn fra en streng inneb\xE6rer \xE5 fjerne de\
  \ ekstra innpakningene for \xE5 f\xE5 tak i den rene teksten inni."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
Elixir har ingen innebygd 'fjern anførselstegn'-funksjon, men det er enkelt å rulle din egen med mønsterpassing eller `String`-funksjoner. Se disse kodestykkene:

```elixir
# Ved bruk av mønsterpassing
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Eksempel på bruk
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# Ved bruk av String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Eksempel på bruk
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

Output for begge metodene vil være:
```
"Hello, World!"
```

## Dypdykk
I gamle dager var anførselstegn i strenger et minefelt—håndter dem feil, og bum, syntaksfeil eller sikkerhetshull. I Elixir behandler mønsterpassing dine strenger som Lego-klosser, lar deg plukke dem fra hverandre og gjenoppbygge med presisjon. Dens robuste `String`-modul er også hendig, fleksibelt fjerner anførselstegn med `trim`-funksjoner. Alternativene? Regulære uttrykk kan sparke anførselstegn til dørkanten, og eksterne biblioteker kan pakke ekstra slagkraft hvis du trenger mer enn grunnleggende stripping.

## Se også
Dypdykk med disse:
- [Elixirs String-modul](https://hexdocs.pm/elixir/String.html)
- [Lær mer om mønsterpassing i Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Regulære uttrykk i Elixir (Regex-modulen)](https://hexdocs.pm/elixir/Regex.html)
