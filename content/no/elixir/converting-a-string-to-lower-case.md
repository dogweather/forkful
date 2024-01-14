---
title:                "Elixir: Konvertering av streng til små bokstaver"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en tekststreng til små bokstaver (lower case) er en vanlig oppgave i mange programmeringsspråk, inkludert Elixir. Dette kan være nyttig for å sikre ensartethet og unngå feil når man sammenligner eller behandler tekststrenger.

# Hvordan

For å konvertere en tekststreng til små bokstaver i Elixir, kan man bruke funksjonen `String.downcase/1`:

```Elixir
iex> String.downcase("ELIXIR")
"elixir"
```

Man kan også bruke den til å konvertere en hele liste med tekststrenger samtidig ved å bruke `Enum.map/2`:

```Elixir
iex> list = ["PROGRAMMERING", "ER", "GØY"]
iex> Enum.map(list, &String.downcase/1)
["programmering", "er", "gøy"]
```

# Dypdykk

Når man bruker `String.downcase/1`, vil den kontrollere hver enkelt bokstav i tekststrengen og erstatte alle store bokstaver med tilsvarende små bokstaver. Dette inkluderer også spesielle bokstaver i forskjellige alfabet, som for eksempel Æ, Ø og Å i det norske alfabetet. Det er viktig å være oppmerksom på at denne funksjonen kun konverterer bokstaver og ikke andre tegn, som tall eller symboler.

# Se også

- Offisiell Elixir dokumentasjon: https://hexdocs.pm/elixir/String.html#downcase/1
- "Gjør deg kjent med Elixir" kurs (på norsk): https://www.udemy.com/course/gjor-deg-kjent-med-elixir/?referralCode=A74A95FA9E95006E8327