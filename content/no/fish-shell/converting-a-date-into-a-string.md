---
title:                "Å konvertere en dato til en streng"
html_title:           "Fish Shell: Å konvertere en dato til en streng"
simple_title:         "Å konvertere en dato til en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en dato til en streng er en vanlig oppgave for programmerere. Dette betyr rett og slett å gjøre om en dato (som kan være i form av tall eller tekst) til en mer lesbar og forståelig format som tekststreng. Dette er nyttig når man for eksempel skal vise datoen i et brukergrensesnitt eller når man skal lagre datoen i en database.

## Hvordan:

I Fish Shell kan du enkelt konvertere en dato til en streng ved å bruke kommandoen `date` etterfulgt av `%Y-%m-%d` for å spesifisere ønsket format. Her er et eksempel på en dato konvertert til en streng:

```Fish Shell
date -f "%Y-%m-%d" 2019-04-17
```
Eksempel på output: `2019-04-17`

Du kan også inkludere klokkeslett ved å legge til `%H:%M:%S`, som vist i eksempelet under:
```Fish Shell
date -f "%Y-%m-%d %H:%M:%S" 2019-04-17T14:30:00
```
Eksempel på output: `2019-04-17 14:30:00`

## Dykk dypere:

Konvertering av dato til streng har vært en viktig oppgave helt siden datamaskiner ble brukt til å håndtere datoer. Tidligere var det vanlig å bruke ASCII-tegn som representerer tall til å lage datoer, men med utviklingen av mer komplekse datamaskiner ble det nødvendig å standardisere formater for datoer. Alternativer til hvordan man kan konvertere datoer til strenger inkluderer biblioteker som Moment.js og PHP's date-funksjon.

For å konvertere en dato i Fish Shell, bruker programmet `strftime` som er en del av PHP, et populært programmeringsspråk som ofte brukes i webutvikling.

## Se også:

- [Fish Shell sin offisielle dokumentasjon om `date` kommandoen](https://fishshell.com/docs/current/cmds/date.html)
- [Moment.js dokumentasjon for å konvertere datoer til strenger](https://momentjs.com/docs/)
- [PHP sin `date` funksjon dokumentasjon](https://www.php.net/manual/en/function.date.php)