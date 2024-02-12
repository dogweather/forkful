---
title:                "Fjerne anførselstegn fra en streng"
aliases:
- /no/elixir/removing-quotes-from-a-string/
date:                  2024-01-26T03:38:39.543946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å strippe anførselstegn fra en streng innebærer å fjerne de ekstra innpakningene for å få tak i den rene teksten inni. Programmerere gjør dette for å sanere input, unngå feil og forberede data til bearbeiding der anførselstegn er hinder, ikke funksjoner.

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
