---
title:                "Å skrive til standardfeil."
html_title:           "Gleam: Å skrive til standardfeil."
simple_title:         "Å skrive til standardfeil."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Skriver du kode og blir frustrert når det ikke fungerer som det skal? Da kan skriving til standard error være løsningen for deg! Her får du en enkel guide til hvordan du kan bruke Gleam til å skrive feilmeldinger til standard error.

## Hvordan

```Gleam
let feilmelding = "Det har oppstått en feil."
error.print(feilmelding)
```

Dette enkle kodesnippet vil skrive feilmeldingen "Det har oppstått en feil" til standard error. Du kan også skrive variabler eller annen informasjon til standard error ved å inkludere dem i feilmeldingen. Dette er nyttig når du trenger å få mer informasjon om hva som gikk galt i koden din.

### Feilmeldinger med tegnsett

Noen ganger kan det oppstå feil som krever spesifikke tegnsett, for eksempel når du jobber med internasjonale karakterer. Du kan enkelt skrive feilmeldinger med dette i Gleam ved å bruke `error.print_utf8` eller `error.print_ascii`.

### Deaktivering av standard error

Hvis du ikke vil ha feilmeldinger som skrives til standard error, kan du enkelt deaktivere dette ved å bruke `error.ignore()`.

## Dypdykk

Skriving til standard error kan være nyttig for feilhåndtering og debugging i koden din. Det er en enkel måte å få mer informasjon om hva som gikk galt og hvor i koden feilen oppsto. Det kan også være nyttig for å sikre at programmet ditt kjører feilfritt og at eventuelle feil blir fanget opp.

## Se også

- [Gleam dokumentasjon om error](https://gleam.run/crash-reporting)
- [Artikkel om håndtering av feil i Gleam](https://dev.to/gleam/a-beginners-guide-to-error-handling-in-gleam-2jji)