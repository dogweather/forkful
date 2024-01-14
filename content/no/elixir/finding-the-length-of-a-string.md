---
title:                "Elixir: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden av en streng er en grunnleggende og vanlig oppgave i mange programmeringsspråk. Dette er nyttig når du jobber med tekstbehandling, dataanalyse eller bare trenger å vite hvor mange tegn som er i en tekststreng.

## Slik gjør du det

For å finne lengden på en streng i Elixir, kan du bruke den innebygde funksjonen `String.length()`. Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen.

```Elixir
iex> String.length("Hei, dette er en test")
19
```

Som du ser, returnerer `String.length()` verdien 19 fordi strengen består av 19 tegn. Det er viktig å merke seg at mellomrom og spesialtegn også telles som tegn.

Når vi bruker denne funksjonen, må vi passe på å ikke bruke en variabel som ikke er en streng som argument. Hvis vi for eksempel prøver å passe inn et tall i `String.length()`, vil vi få en feilmelding.

```Elixir
iex> String.length(123)
** (ArgumentError) argument error
```

For å unngå dette, kan vi sikre at vi bare bruker strengvariabler som argument.

## Dykk dypere

Elixir er et funksjonelt programmeringsspråk, noe som betyr at funksjoner er sentralt og kan betraktes som verdifulle ressurser. Det er derfor viktig å vite hvordan de fungerer og hvordan de kan brukes for å optimalisere koden din.

Når du bruker `String.length()`, må du vite at funksjonen egentlig bare teller antall bytes i en streng. Dette kan føre til uventede resultater hvis du håndterer flerspråklige tegn eller emojis som bruker flere bytes.

For å unngå dette, kan du bruke funksjonen `String.codepoints()` som splitter strengen i en liste av unicode-tegn. Deretter kan du bruke `Enum.count()` til å telle antall elementer i denne listen.

```Elixir
iex> "Hello, world!" |> String.codepoints() |> Enum.count()
13
```

## Se også

- [Offisiell Elixir dokumentasjon om `String.length()`](https://hexdocs.pm/elixir/String.html#length/1)
- [Offisiell Elixir dokumentasjon om `String.codepoints()`](https://hexdocs.pm/elixir/String.html#codepoints/1)
- [Offisiell Elixir dokumentasjon om `Enum.count()`](https://hexdocs.pm/elixir/Enum.html#count/1)