---
title:    "Fish Shell: Leser en tekstfil"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Et typisk problem for programmerere er å lese og behandle store mengder tekstfiler. Enten det er loggfiler, konfigurasjonsfiler eller andre typer datafiler. En effektiv måte å håndtere disse filene på er å bruke et kommandolinjeverktøy som Fish Shell. I denne bloggposten vil vi se på hvordan du kan bruke Fish Shell for å lese tekstfiler og behandle dataene på en enkel måte.

## Hvordan

Fish Shell har flere innebygde funksjoner for å lese og behandle tekstfiler. La oss se på et eksempel hvor vi ønsker å finne antall ganger et bestemt ord forekommer i en tekstfil. Vi kan bruke kommandoen "count" sammen med "grep" for å oppnå dette. Det hele kan se slik ut:

```Fish Shell
grep -o "<ord>" <tekstfil> | count -l
```

Dette vil returnere antall linjer som inneholder det spesifiserte ordet i tekstfilen. Hvis vi for eksempel ønsker å finne antall ganger ordet "Fish" forekommer i filen "tekst.txt", vil kommandoen se slik ut:

```Fish Shell
grep -o "Fish" tekst.txt | count -l
```

Vi kan også bruke Fish Shell for å sortere og filtrere dataene i en tekstfil. La oss si at vi har en tekstfil med navnene til ansatte i et firma. Vi ønsker å sortere navnene alfabetisk og kun vise de ansatte som har stilling som "leder". Dette kan gjøres ved å kombinere "sort" og "grep" kommandoene sammen, som vist nedenfor:

```Fish Shell
sort <ansattliste.txt> | grep "leder"
```

Dette er bare noen enkle eksempler på hvordan Fish Shell kan hjelpe oss med å lese og behandle tekstfiler. Mulighetene er mange og avhenger av hva slags data vi ønsker å håndtere.

## Dypdykk

For de som ønsker å lære mer om å lese tekstfiler med Fish Shell, er det flere funksjoner og kommandoer man kan utforske. For eksempel har Fish Shell en "head" kommando som viser de første linjene i en tekstfil, og en "tail" kommando som viser de siste linjene. Det finnes også muligheter for å kombinere forskjellige kommandoer og lage komplekse skript for å håndtere tekstfiler.

## Se også

- [Fish Shell offisiell nettside](https://fishshell.com/)
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Kommandoer og funksjoner i Fish Shell](https://fishshell.com/docs/current/cmds.html)