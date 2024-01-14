---
title:    "Fish Shell: Konvertering av dato til en streng"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng kan være nyttig når du vil vise en dato på en mer lesbar måte. Dette er spesielt nyttig når du jobber med datoer i tekstfiler eller programmeringsspråk.

## Hvordan

For å konvertere en dato til en streng i Fish Shell, kan du bruke kommandoen `date -s`. Her er et eksempel på hvordan du kan gjøre det:

```
Fish Shell> date -s "2021-05-24"
Mandag 24. Mai 2021  00:00:00
```

Som du kan se, viser kommandoen datoen på en mer lesbar måte ved å bruke navn på måned og ukedag.

Du kan også bruke ulike formateringsalternativer for å tilpasse utseendet på datoen. Her er et eksempel der vi endrer formatet til å vise datoen som dag måned/år:

```
Fish Shell> date -s "2021-05-24" "+%d %B/%Y"
24 Mai/2021
```

Du kan også bruke flere kommandoer og variabler for å tilpasse datoen ytterligere. For eksempel, for å bare vise dag, måned og år, kan du bruke `date +%d.%m.%Y`:

```
Fish Shell> date -s "2021-05-24" +%d.%m.%Y
24.05.2021
```

## Dypdykk

Bak kulissene bruker Fish Shell kommandolinjeverktøyet `date` som er tilgjengelig på mange Unix-baserte operativsystemer. Denne kommandoen lar deg manipulere og vise datoen på forskjellige formater. For mer informasjon og en fullstendig liste over formateringsalternativer, kan du bruke kommandoen `man date` i terminalen.

Nå som du vet hvordan du konverterer en dato til en streng i Fish Shell, er du klar til å bruke dette verktøyet i dine egne prosjekter for å vise datoer på en mer lesbar måte.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Unix `date` kommando dokumentasjon](https://www.man7.org/linux/man-pages/man1/date.1.html)
- [Guide til Grunnleggende Styresystemkonfigurasjon og Verktøy](https://www.ntnu.no/wiki/display/itinfo/010+Guide+til+Grunnleggende+Styresystemkonfigurasjon+og+Verkt%C3%B8y)