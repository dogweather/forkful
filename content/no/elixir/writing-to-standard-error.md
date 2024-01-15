---
title:                "Å skrive til standardfeil"
html_title:           "Elixir: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig når du vil få ut ekstra informasjon fra programmet ditt til terminalen. Det er også en måte å skille mellom feilmeldinger og vanlige utskrifter.

## Hvordan gjøre det

For å skrive til standard error i Elixir, kan du bruke funksjonen `IO.puts/2` og sette `:stderr` som første argument. La oss se på et eksempel:

```Elixir
IO.puts(:stderr, "Dette er en feilmelding!")
```

Dette vil skrive ut teksten "Dette er en feilmelding!" til standard error. Her er en annen variant som inkluderer en variabel:

```Elixir
feilmelding = "Kan ikke åpne filen."
IO.puts(:stderr, "Feil: #{feilmelding}")
```

Denne gangen vil teksten "Feil: Kan ikke åpne filen." bli skrevet ut til standard error. Du kan også bruke `IO.write/2` funksjonen på samme måte for å skrive ut tekst uten å legge til noen nye linjer på slutten.

## Dypdykk

Standard error er en del av det såkalte "file descriptors" systemet som brukes i operativsystemet. Når et program kjører, blir det tildelt tre file descriptors: standard input (`:stdin`), standard output (`:stdout`) og standard error (`:stderr`).

Standard error brukes vanligvis for å skrive feilmeldinger og andre viktige meldinger som brukeren trenger å vite om. Det er også vanlig å omdirigere standard error til en loggfil i stedet for å skrive ut det til terminalen, noe som kan være nyttig når programmet kjøres i bakgrunnen.

## Se også

- Elixir `IO` modul [dokumentasjon](https://hexdocs.pm/elixir/IO.html)
- Elixir `Kernel` modul [dokumentasjon](https://hexdocs.pm/elixir/Kernel.html)