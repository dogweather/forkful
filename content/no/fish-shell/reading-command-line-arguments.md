---
title:                "Fish Shell: Lesing av kommandolinje-argumenter"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvorfor du bør lære deg å lese kommandolinje-argumenter i Fish Shell? Det kan være en svært nyttig ferdighet når du jobber med kommandolinje-programmer, og kan hjelpe deg med å automatisere og effektivisere oppgaver.

## Slik gjør du det

Det er enkelt å lese kommandolinje-argumenter i Fish Shell. Alt du trenger å gjøre er å bruke innebygde kommandoer som "argv" og "$argv". La oss se på et eksempel:

```Fish Shell
$ fish script.sh argument1 argument2
```

For å lese disse argumentene i Fish Shell, kan du bruke følgende kommandoer:

```Fish Shell
echo $argv[1]
```

Denne koden vil gi deg output av "argument1" i dette tilfellet. Hvis du ønsker å hente ut alle argumentene, kan du bruke følgende kode:

```Fish Shell
echo $argv
```

Dette vil gi deg output av "argument1 argument2", som er alle kommandolinje-argumentene som ble gitt til scriptet ditt.

## Dykk dypere

Nå som vi har sett på det grunnleggende, kan vi ta en dypere titt på å lese kommandolinje-argumenter i Fish Shell. Det finnes flere måter å lese og behandle argumenter på, som for eksempel å bruke "for"-løkker og "switch"-kommandoer. Det er også viktig å huske på at kommandolinje-argumenter kan være nyttige for å gi input til skriptet ditt, for eksempel ved å angi en filsti som argument.

## Se også

- [Offisiell dokumentasjon for Fish Shell](https://fishshell.com/docs/current/)
- [En guide for grunnleggende Fish Shell kommandoer](https://fishshell.com/docs/current/tutorial.html)
- [En introduksjon til kommandolinjen for nybegynnere](https://skillcrush.com/blog/command-line-basics/)