---
title:                "Gleam: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Man kan lure på hvorfor noen ville engasjere seg i skriving til standard error i programmering. Svaret er enkelt - dette er en viktig måte å melde feil og problemer tilbake til brukeren av programmet ditt. Ved å skrive til standard error, kan du sørge for at brukeren blir oppmerksom på eventuelle problemer som oppstår under kjøringen av programmet, og dette kan hjelpe deg med å feilsøke og forbedre koden din.

## Slik gjør du det
For å skrive til standard error i Gleam, bruker du funksjonen `log.error()` og inkluderer en melding som parameter. La oss si at du ønsker å rapportere en feil i forbindelse med mottak av brukerens input. Du kan gjøre det på følgende måte:

```Gleam
let feilmelding = "Feil ved mottak av input fra brukeren"
log.error(feilmelding)
```

Dette vil skrive ut meldingen "Feil ved mottak av input fra brukeren" til standard error. Det er også mulig å inkludere variabler i meldingen, for eksempel:

```Gleam
let tall = 42
let feilmelding = "Feil! Fant ikke tallet {{tall}}" #[out("feil", value: tall)]
log.error(feilmelding)
```

Dette vil skrive ut meldingen "Feil! Fant ikke tallet 42" til standard error.

## Dypdykk
Nå som du vet hvordan du skriver til standard error, kan det være nyttig å vite litt mer om hvordan dette fungerer i Gleam. Når du kaller `log.error()` funksjonen, blir meldingen sendt videre til standard error strømmen, som er en strøm spesifikt for å rapportere feil og statusmeldinger. Du kan også få tilgang til standard error strømmen direkte i din Gleam kode ved å bruke `sys.get_stderr()` funksjonen.

Det er viktig å merke seg at meldinger som skrives til standard error vil vises i terminalen eller konsollen, og ikke i den vanlige output strømmen. Dette gjør det til en nyttig måte å skille feilmeldinger og statusmeldinger fra vanlig output.

## Se også
- [Offisiell Gleam dokumentasjon for log modulen](https://gleam.run/book/standard-library.html#log)
- [Meldingstyper som støttes i log modulen](https://gleam.run/reference/std-lib/log.html#message-components)
- [Mer om standard error strømmen i Unix/Linux miljøer](https://www.tutorialspoint.com/unix/unix-io-redirections.htm)