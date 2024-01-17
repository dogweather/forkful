---
title:                "Interpolering av en streng"
html_title:           "Elixir: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å interpolere en streng i programmering betyr å erstatte en del av en tekststreng med en verdi. Dette gjøres vanligvis ved å bruke såkalte "placeholders" som forteller programmet hvor verdien skal settes inn. Interpolering brukes ofte for å gjøre teksten mer dynamisk eller for å sette sammen komplekse tekster.

## Slik gjør du det:

For å interpolere en streng i Elixir, kan du bruke funksjonen `String.interpolate/2` eller den mer vanlige syntaksen `#{...}`. Her er et eksempel på begge deler:

```Elixir
navn = "Ola"
alder = 30

# String.interpolate/2 syntaks:
tekst = String.interpolate("Hei, mitt navn er %{navn} og jeg er %{alder} år gammel.", [navn: navn, alder: alder])
# => Hei, mitt navn er Ola og jeg er 30 år gammel.

# #{...} syntaks:
tekst = "Hei, mitt navn er #{navn} og jeg er #{alder} år gammel."
# => Hei, mitt navn er Ola og jeg er 30 år gammel.
```

## Dykk dypere:

Interpolering ble først introdusert i programmeringsspråket Perl og har siden blitt et vanlig konsept i mange moderne språk. I Elixir er det også mulig å bruke `~S` og `~N` sigils for å interpolere strenger. Det finnes også alternative løsninger for interpolering, som for eksempel å kombinere strenger med `<>` operatøren.

For å implementere interpolering i Elixir, brukes en kombinasjon av funksjoner som `String.replace/3` og regulære uttrykk. Dette gjør det mulig å sette inn verdier på bestemte steder i en streng uten å måtte gjøre manuelle endringer.

## Se også:

- Offisiell Elixir dokumentasjon for `String.interpolate/2` funksjonen: https://hexdocs.pm/elixir/String.html#interpolate/2
- En bloggpost som går dypere inn i Elixir strenginterpolering: https://beneathdata.com/blog/elixir-string-interpolation/