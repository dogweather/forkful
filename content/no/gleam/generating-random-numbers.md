---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Generering av tilfeldige tall er prosessen med å produsere en sekvens av tall som ikke har noe mønster eller forutsigbarhet. Programmerere gjør det for å simulere ikke-deterministiske hendelser i systemer, bedre data sikkerhet osv.

## Hvordan:

Her er en måte du kan generere tilfeldige tall på i Gleam:

```Gleam
import gleam/otp/random.{int, float}

fn demo_random_numbers() {
  let _ = int(1, 100)   // Gir deg et tilfeldig tall mellom 1 og 100
  let _ = float()       // Gir deg et tilfeldig flyttall mellom 0.0 og 1.0
}
```
Eksempel utdata kan være noe slik:

```Gleam
demo_random_numbers() => 42, 0.8159627499788994
```
Husk, resultatene vil variere hver gang du kjører funksjonen, det er et poeng med tilfeldighet!

## Dypdykk:

Historisk sett har generering av tilfeldige tall vært en kritisk del av mange datamaskinprogrammer, fra spill til sikkerhetssystemer. Alternativene inkluderer andre tilfeldige tallgeneratorer som pseudorandom nummergeneratorer (PRNGs), eller høynivå biblioteker som `random`.

Detaljer om Gleams implementering er ganske rett frem. Gleam bruker OTP's standard `:rand.uniform/0` og `:rand.uniform/1` funksjonene for å generere tilfeldige flyttall og hele tall.

## Se Også:

For mer informasjon om Gleam og generering av tilfeldige tall, se på disse kildene:

- Gleam's offisielle dokumentasjon: https://hexdocs.pm/gleam_stdlib/gleam/otp/random/
- Elixir's `:rand` modul: https://hexdocs.pm/elixir/1.12/Kernel.html#functions
- Mer om tilfeldige tallgeneratorer: https://en.wikipedia.org/wiki/Random_number_generation