---
title:                "Å bruke regulære uttrykk"
html_title:           "Elixir: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions er kraftige verktøy som gjør det mulig å søke, manipulere og validere tekststrenger basert på et gitt mønster. Dette er spesielt nyttig for å håndtere tekstinput i programmering, slik som å finne en bestemt e-postadresse eller validere et telefonnummer. Mange programmerere bruker regulære uttrykk for å forenkle oppgaver og håndtere kompleks tekstbehandling.

## Hvordan:
I Elixir, bruker vi regulære uttrykk ved å bruke modulen Regex. For å søke etter et gitt mønster i en tekststring, bruker vi funksjonen `Regex.match?(pattern, string)` hvor "pattern" er det vi ønsker å søke etter og "string" er tekststrengen vi vil søke gjennom. Her er et eksempel på hvordan vi kan finne alle ord som begynner med "h" i en tekststreng:

```Elixir
text = "Hello world, how are you?"
Regex.match?(/h\w+/, text)
```

Dette vil returnere `true` siden teksten inneholder ordene "Hello" og "how".

## Dypdykk:
Regular expressions ble først utviklet på 1950-tallet og har siden blitt en standard i mange programmeringsspråk. Det finnes også alternative metoder for å håndtere tekststrenger, som for eksempel string-matching algoritmer, men regulære uttrykk er fortsatt svært populære og kraftige verktøy i programmering.

Når vi bruker regulære uttrykk i Elixir, blir de kompilert til NFA (Nondeterministic Finite Automaton) og utført veldig effektivt. Dette gjør Elixir og andre språk som bruker regulære uttrykk ideelle for store tekstbehandlingsoppgaver.

## Se også:
- [Documentation for Elixir Regex module](https://hexdocs.pm/elixir/Regex.html)
- [Online regular expression tester](https://regex101.com/) for å øve og teste ut regulære uttrykk på ulike tekststrenger.