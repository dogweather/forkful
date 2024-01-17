---
title:                "Hämta aktuellt datum"
html_title:           "Bash: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att få den aktuella datumet är en viktig del av programmering eftersom det innebär att du kan få ditt program att fungera på ett tidsbestämt sätt. Detta kan vara användbart för att schemalägga uppgifter, logga händelser eller helt enkelt visa aktuell datum till användaren.

## Hur:

För att få den aktuella datumet i Bash, används kommandot `date`. Det kan också användas för att visa tider och tidszoner.

```Bash
date +%d/%m/%y
```
Detta kommer att visa datumet i formatet "dd/mm/åå". %d, %m och %y står för dag, månad och år.

```Bash
date +%A
```
Detta kommer att visa veckodagen som idag är, t.ex. torsdag.

## Deep Dive:

`date` kommandot är en del av GNU Core Utilities paket som tillhandahålls av det GNU-projektet. Det första publicerade versionen av paketet inkluderade datokommandot och släpptes redan år 1983.

Om du inte vill använda `date` kommandot kan du också använda en inbyggd variabel i Bash som heter `$EPOCHSECONDS`. Detta ger antalet sekunder sedan 1 januari 1970 (Unix-epoken) och kan därefter formateras på önskat sätt.

Installation av GNU Core Utilities är inte nödvändigt eftersom de ofta redan finns förinstallerade på Unix-liknande system. För Windows-användare finns CoreUtils installerat som en del av "Bash on Ubuntu on Windows" funktionen.

## Se även:

- [GNU Core Utilities](https://www.gnu.org/software/coreutils/coreutils.html)
- [EPOCHSECONDS variabeln](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bash on Ubuntu on Windows](https://docs.microsoft.com/en-us/windows/wsl/install-win10)