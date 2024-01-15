---
title:                "Skriving til standardfeil"
html_title:           "Fish Shell: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive til standardfeil (standard error) kan hjelpe deg med å feilsøke og forbedre koden din. Ved å skrive feilmeldinger og annen viktig informasjon til standardfeil, kan du enkelt fange og håndtere feil i programmet ditt.

## Hvordan gjør du det
Å skrive til standardfeil i Fish Shell er enkelt. Du kan bruke kommandoen "echo" etterfulgt av teksten du vil skrive til standardfeil. For eksempel:

```
fish shell -c 'echo "Dette er en feilmelding" >&2'
```

Dette vil skrive "Dette er en feilmelding" til standardfeil. Legg merke til at "&2" brukes for å sende teksten til standardfeil istedenfor standardutgang (standard output).

En annen nyttig måte å skrive til standardfeil på er å bruke kommandoen "printf". Denne kommandoen lar deg formatere teksten du skriver til standardfeil. For eksempel:

```
fish shell -c 'printf "Det er %s feil i koden din" "3" >&2'
```

Dette vil skrive "Det er 3 feil i koden din" til standardfeil.

## Dypdykk
Å bruke kommandoen ">&2" lar deg sende teksten direkte til standardfeil. Men hva betyr dette egentlig? I Fish Shell er standardutgang (standard output) den standarden som brukes for å vise informasjon til brukeren, mens standardfeil (standard error) er den standarden som brukes for å vise feilmeldinger og annen viktig informasjon.

Ved å sende teksten direkte til standardfeil, vil den bli vist uavhengig av eventuelle andre kommandoer som kan gjøre endringer på standardutgangen. Dette gjør at du kan være sikker på at feilmeldingene dine vil bli vist til brukeren.

## Se Også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/) 
- [Echo kommando dokumentasjon](https://fishshell.com/docs/current/cmds/echo.html)
- [Printf kommando dokumentasjon](https://fishshell.com/docs/current/cmds/printf.html)