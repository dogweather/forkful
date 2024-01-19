---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Leser kommandolinje-argumenter med Gleam

## Hva & Hvorfor?

Kommandolinje-argumenter er informasjon gitt til et program når det startes. Programmerere bruker dette til å kontrollere oppførselen til et program fra starten, uten brukerinteraksjon.

## Hvordan gjør man det:

Gleam har innebygd støtte for å lese kommandolinje-argumenter ved hjelp av `os.args` funksjonen. Her er et eksempel:

```gleam
import gleam/os

fn main(_) {
  let arguments = os.args()
  case arguments {
    [] -> io.println("Ingen argumenter ble gitt")
    args -> io.println("Følgende argumenter ble gitt: ", args)
  }
}
```
Når du kjører dette programmet, vil output være forskjellig basert på kommandolinje-argumentene du gir. For eksempel:

```bash
$ gleam run main -- argument1 argument2
Følgende argumenter ble gitt: ["argument1", "argument2"]
```

## Dypdykk

Kommandolinjeargumenter stammer fra Unix-æraen, hvor kommandolinjestyring var den dominerende måten å betjene datamaskiner på. Gleam sin implementasjon av kommandolinjeargumenter er i tråd med andre moderne programmeringsspråk, som Ruby og Python, men med sin egen tweek for enklere bruk.

Et alternativ til å bruke `os.args` kan være å legge inn konfigurasjonsinnstillinger i en fil og lese den filen ved oppstart. Denne metoden er mer vanlig for større programmer, der det antallet innstillinger kan være omfattende.

Implementeringen av `os.args` i Gleam skjer ved å hente verdien av `ARGV` globalvariabelen i Erlang, behandlet som en liste av strenger, i samme rekkefølge som de ble gitt til processen.

## Se også

- Utforsk [Gleam documentation](https://gleam.run/docs/) for mer informasjon om `os.args` og andre nyttige biblioteker.
- For en dypere dykk inn i kommandolinjeargumenter og deres historie, sjekk ut [Command Line Arguments (Wikipedia)](https://en.wikipedia.org/wiki/Command-line_argument_parsing) og [Command Line Arguments in Erlang](https://erlang.org/doc/man/init.html#id226226).