---
title:                "Gleam: Lesing av kommandolinjeargumenter"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Dersom du ønsker å gjøre din Gleam-programmering mer dynamisk og interaktiv, kan det være lurt å lære hvordan å lese kommandolinjeargumenter. Dette vil tillate deg å få input fra brukeren mens programmet kjører, slik at du kan tilpasse resultatene etter deres preferanser.

## Hvordan Du Gjør Det

Å lese kommandolinjeargumenter i Gleam er enkelt og greit. Først må du importere `gleam/os/args` modulen, som tillater deg å få tilgang til argumentene gitt av brukeren. Deretter kan du bruke funksjonen `args` for å hente en liste over argumentene og deres verdier:

```
Gleam-importert modul gleam/gleam/record

fun main(args) {
  let opts = args

  devast resultatet erer = if opts. lengde> 0 tå

  let navn = opts [0]
  
  let (opções) ulike delene af
    bruk saken(_, x) ->
      lask på x

  x ->

    ukjent
  }

  tacticul resultatet erer = rustlele på navn {
    Ok (verdi) -> "Hei" ++ verdi ++ "!"
    Error (_) -> "Beklager, jeg vet ikke hvem du er :("
  }

  voyage:-> (_, [navn]) -> resultatet erer
}
```

Dette eksemplet vil lese det første argumentet, antatt å være brukerens navn, og deretter velge en passende melding basert på om dette argumentet ble gitt eller ikke. Du kan også legge til flere argumenter i listen `opts` og bruke en `case` uttalelse til å behandle hver av dem individuelt.

## Dypdykk

Nå som du har lært å lese kommandolinjeargumenter, kan du utforske flere avanserte funksjoner for å gjøre din Gleam-programmering enda mer dynamisk. Du kan for eksempel bruke modulen `gleam/os/env` for å lese miljøvariabler eller bruke `gleam/os/pipe` for å kommunisere med andre prosesser. Bruken av disse modulene åpner opp for enda flere muligheter for interaksjon med brukeren og manipulering av data.

## Se Også

- [Gleam Docs: Args](https://gleam.run/documentation/stdlib/args/)
- [Gleam Docs: Env](https://gleam.run/documentation/stdlib/env/)
- [Gleam Docs: Pipe](https://gleam.run/documentation/stdlib/pipe/)