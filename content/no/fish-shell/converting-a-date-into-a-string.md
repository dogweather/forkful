---
title:                "Fish Shell: Konvertere en dato til en streng"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere står overfor utfordringen med å konvertere en dato til en streng i sine programmer. Dette kan være nyttig for å vise datoer på en mer brukervennlig måte eller for å lagre datoer i en database. I denne bloggposten vil jeg vise deg hvordan du kan konvertere datoer til strenger ved hjelp av Fish Shell.

## Slik gjør du det
For å konvertere en dato til en streng i Fish Shell, kan du bruke kommandoen `date`. Dette vil gi deg dagens dato i riktig format. La oss si at vi ønsker å konvertere datoen 29. august 2021 til en streng, da kan vi bruke følgende kommando:

```Fish Shell
date -d '2021-08-29' '+%d. %B %Y'
```

Dette vil gi oss følgende output:

```Fish Shell
29. august 2021
```

Hvis du ønsker å konvertere en dato som allerede er i Fish Shell, kan du bruke kommandoen `date` sammen med variabelen `$date`. La oss si at vi har en variabel `$today` som inneholder dagens dato, da kan vi bruke følgende kommando:

```Fish Shell
date -d $today '+%d. %B %Y'
```

Dette vil gi oss samme output som i eksempelet over.

## Dykk dypere
Dette er bare et enkelt eksempel på hvordan du kan konvertere en dato til en streng i Fish Shell. Det finnes mange muligheter og formater du kan bruke. Du kan lese mer om kommandoen `date` i Fish Shell sin [dokumentasjon](https://fishshell.com/docs/3.3/cmds/date.html). Det er også verdt å utforske andre måter å konvertere datoer på ved hjelp av andre funksjoner og verktøy tilgjengelig i Fish Shell.

## Se også
- [Fish Shell dokumentasjon for datokonvertering](https://fishshell.com/docs/3.3/cmds/date.html)
- [Tutorial: Konvertering av datoer i Fish Shell](https://www.baeldung.com/linux/date-command)

Takk for at du leste denne bloggposten om å konvertere datoer til strenger i Fish Shell. Hvis du har noen spørsmål eller kommentarer, ikke nøl med å legge igjen en kommentar nedenfor. Lykke til med kodingen!