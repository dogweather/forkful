---
title:                "Fish Shell: Å skrive til standardfeil"
simple_title:         "Å skrive til standardfeil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig del av feilsøking og debugging i Fish Shell. Det lar deg få informasjon om eventuelle feil eller problemer som oppstår under kjøring av et program.

## Slik gjør du det

For å skrive til standard error kan du bruke kommandoen `echo` etterfulgt av `.`, som representerer standard error-strømmen. For eksempel:

```
Fish Shell $ echo "Dette er en feilmelding" 1>&2
```

Dette vil skrive ut teksten "Dette er en feilmelding" til standard error, som kan sees ved å bruke `2>` operatøren. Her er et eksempel på hvordan dette ser ut i terminalen:

```
Dette er en feilmelding
```

## Dypdykk

Når du skriver til standard error, er det viktig å vite forskjellen mellom standard error og standard output. Standard error brukes spesifikt for feilmeldinger og kan skille seg fra standard output som brukes til normal utskrift av informasjon.

Du kan også kombinere skriving til standard error og standard output ved å bruke `2>&1` operatøren. Dette vil sende både standard error og standard output til det samme stedet. For eksempel:

```
Fish Shell $ echo "Feilmelding" 2>&1
Feilmelding
```

## Se også

- [Fish Shell offisiell nettside](https://fishshell.com/)
- [Dokumentasjon for å skrive til standard error i Fish Shell](https://fishshell.com/docs/current/tutorial.html#error-output)
- [En oversikt over Fish Shell kommandoer og syntax](https://fishshell.com/docs/current/commands.html)