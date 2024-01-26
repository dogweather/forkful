---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard error (stderr) betyr å sende feil og diagnostiske meldinger til en separat utstrøm. Programmerere gjør dette for å skille vanlig utdata (stdout) fra feil, for lettere feilsøking og logging.

## Slik gjør du:
I Gleam bruker du `io`-modulen for å skrive til stderr. Her er hvordan:

```gleam
import gleam/io

pub fn main() {
  io.stderr_print("Dette er en feilmelding\n")
}
```

Når du kjører denne koden, vil du se output i terminalen:

```
Dette er en feilmelding
```

## Dypdykk:
I eldre programmeringsspråk som C, ble stderr brukt til å skille utdiagnostiske meldinger. I Gleam og mange moderne språk er prinsippet det samme. Alternativer inkluderer logging-biblioteker eller rammerverk som kan gi mer funksjonalitet rundt loggbehandling. Intern implementasjonsdetalj for Gleam sin `io.stderr_print` bruker Erlang sin innebygde I/O funksjonalitet, siden Gleam kompilerer til Erlang bytecode.

## Se Også:
- Gleam's offisielle dokumentasjon på `io`-modulen: https://hexdocs.pm/gleam_stdlib/gleam/io/
- Erlang's dokumentasjon for feil og utdata håndtering: http://erlang.org/doc/apps/stdlib/io_page.html
- Tutorial på logging i Gleam: [relevant lenke her når tilgjengelig]
