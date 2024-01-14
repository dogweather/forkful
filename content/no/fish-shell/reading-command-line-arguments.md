---
title:                "Fish Shell: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en programmerer eller dataentusiast, er sjansen stor for at du allerede kjenner til viktigheten av å kunne håndtere kommandolinjeargumenter. Dette er en viktig del av å kunne effektivt jobbe med Fish Shell og andre programmeringsspråk på en rask og enkel måte.

## Hvordan
```Fish Shell``` er et populært verktøy som brukes til å håndtere kommandolinjeargumenter. Den har et enkelt syntaks som gjør det mulig å lese og behandle argumenter på en intuitiv måte. Under er et enkelt eksempel på hvordan du kan lese og vise en kommandolinjeargument:

```
# Leser det første argumentet som ble gitt til Fish Shell
set argument $argv[1]

# Skriver ut argumentet til skjermen
echo "Argumentet ditt er: $argument"
```

Om du for eksempel kjører dette skriptet med kommandoen ```fish mittskript.fish hello```, vil du få følgende output: ```Argumentet ditt er: hello```.

## Dykk dypere
Det er flere muligheter når det kommer til å lese kommandolinjeargumenter i Fish Shell. Du kan for eksempel bruke kommandoen ```switch``` for å sjekke om et bestemt argument er gitt eller ikke, og deretter utføre forskjellige handlinger basert på dette. Det er også mulig å bruke flags og options, som gir enda mer fleksibilitet når det gjelder å håndtere argumenter.

Det er også verdt å merke seg at Fish Shell har innebygde variabler som kan gi nyttig informasjon om argumentene som blir gitt til programmet ditt. Dette inkluderer for eksempel ```$argv``` som inneholder alle argumentene og ```$argc``` som inneholder antall argumenter som er gitt.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/argv.html)
- [Tutorial: Using Command-Line Arguments in Fish Shell](https://dzone.com/articles/using-command-line-arguments-in-fish-shell)
- [Advanced Fish Shell Tips and Tricks](https://medium.com/@ruturajv/advanced-fish-shell-tips-and-tricks-7b34426a2f48)