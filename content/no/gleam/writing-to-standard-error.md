---
title:    "Gleam: Skriver til standardfeil"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å skrive til standard error i Gleam, men en av de vanligste er hvis man ønsker å skrive ut feil eller unntak i programmet for å forenkle feilsøking.

## Hvordan gjøre det
For å skrive til standard error i Gleam, kan du bruke funksjonen `gleam/io#stderr/1`. Her er et eksempel på hvordan du kan bruke denne funksjonen i et Gleam-program:

```gleam
let tekst = "Dette er en feilmelding"
gleam/io#stderr(tekst)
```

Dette vil skrive ut teksten "Dette er en feilmelding" til standard error. Du kan også bruke formatering for å bygge en mer kompleks melding, for eksempel:

```gleam
let tall = 42
let melding = "Kan ikke dele på {tall}"
gleam/io#stderr(melding, [tall])
```

Dette vil skrive ut meldingen "Kan ikke dele på 42" til standard error.

## Dypdykk
Når du skriver til standard error i Gleam, vil meldingene automatisk bli skrevet ut i rød farge, slik at de skiller seg ut fra standard output. Dette kan være nyttig for å raskt identifisere feil og unntak når du kjører et program.

Det er også mulig å skrive til standard error ved hjelp av en pipe-operator. For eksempel:

```gleam
let tekst = "Dette er en feilmelding"
tekst |> gleam/io#stderr
```

Dette vil oppnå det samme resultatet som det første eksempelet.

## Se også
- [Gleam dokumentasjon om å skrive til standard error](https://gleam.run/book/tutorials/writing_to_standard_error.html)
- [Gleam dokumentasjon om IO-modulen](https://gleam.run/book/standard_library/io.html)
- [Gleam Slack-gruppen for diskusjon](https://gleam-lang.slack.com/)