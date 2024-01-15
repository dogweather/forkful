---
title:                "Sammenstilling av strenger"
html_title:           "Elixir: Sammenstilling av strenger"
simple_title:         "Sammenstilling av strenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger, også kjent som konkatenasjon, er en grunnleggende og nødvendig ferdighet i Elixir-programmering. Ved å kunne sette sammen flere strenger kan du lage mer dynamiske og tilpassede meldinger, påmeldingsskjemaer og annen tekst i programmene dine.

## Hvordan

```Elixir
# Eksempel 1: Enkel konkatenasjon
"Velkommen til Elixir-programmering " <> "Artikkel!"

# Output: "Velkommen til Elixir-programmering Artikkel!"

# Eksempel 2: Konkatenasjon med variabler
fornavn = "Jens"
etternavn = "Hansen"
"Velkommen, " <> fornavn <> " " <> etternavn <> "!"

# Output: "Velkommen, Jens Hansen!"

# Eksempel 3: Konkatenasjon med interpolering
navn = "Petter"
"Velkommen, #{navn}!"

# Output: "Velkommen, Petter!"
```

## Deep Dive

I Elixir er konkatenasjon utført ved hjelp av operatøren `<>`, som brukes til å kombinere to strenger. Dette kan også brukes til å kjede sammen flere strenger i én operasjon. En viktig egenskap ved konkatenasjon i Elixir er at den er en ikke-destruktiv operasjon, noe som betyr at originale strenger ikke endres, men heller en ny streng blir opprettet.

Det er også viktig å merke seg at konkatenasjon i Elixir er veldig effektiv, siden det er en del av Elixir-datastrukturen, kalt binærer. Dette betyr at konkatenasjon er mye raskere enn i andre språk som behandler strenger som en liste av tegn.

## Se også

- [Offisiell dokumentasjon for konkatenasjon i Elixir](https://hexdocs.pm/elixir/1.11.2/String.html#concatenation/)
- [En artikkel om effektiv strengkonkatenasjon i Elixir](https://tech.europace.de/efficient-string-concatenation-in-elixir/)
- [En diskusjon om strengkonkatenasjon i Elixir-forumet](https://elixirforum.com/t/concatenate-strings-or-binaries/1697/)