---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konvertere en dato til en streng i Fish Shell

## Hva & Hvorfor?
Å konvertere en dato til en streng betyr å endre formatet fra en datatyper til tekst. Programmerere utfører dette for å øke lesbarheten og for å forenkle datalagring og -behandling.

## Hvordan gjøre det:
Du kan bruke unix-dato kommandoen til å konvertere dato til streng i Fish Shell. Her er et eksempel:

```Fish Shell
set -l now (date "+%Y-%m-%d %H:%M:%S")
echo $now
```
Eksemplet ovenfor vil vise en utgang som dette:

```Fish Shell
2022-03-05 14:21:40
```
## Dypdykk
Historisk sett har utviklere skrevet kompliserte algoritmer for håndtering av datoer og tider, men dagens programmeringsspråk og skall som Fish gir innebyggede hjelpere for dette formålet.

Et alternativ til denne tilnærmingen kan være å bruke `strftime`-funksjonen, som gir mer finkornet kontroll over det resultatformatet.

Implementeringsdetaljer spesielle for Fish Shell inkluderer at lokale variabler opprettes med `set -l` tastene, og kommandoer kan inkluderes i parentesene for å skape dem dynamisk.

## Se Også
For mer informasjon, ta en titt på følgende kilder:

2. Dato-kommando detaljer: [https://linux.die.net/man/1/date.](https://linux.die.net/man/1/date.)
3. Forstå grown-ups' datoer og tid: [https://yourbasic.org/golang/format-parse-string-time-date-example/.](https://yourbasic.org/golang/format-parse-string-time-date-example/.)