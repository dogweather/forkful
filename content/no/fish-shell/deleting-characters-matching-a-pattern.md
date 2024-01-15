---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Fish Shell: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, når vi jobber med tekstfiler eller kommandolinjeverktøy, kan det hende vi trenger å fjerne en spesifikk type tegn fra en tekststreng. Dette kan for eksempel være om vi ønsker å fjerne alle tall fra en tekstfil, eller alle spesielle tegn fra en kommandolinjekommando. Å kunne fjerne karakterer som matcher et bestemt mønster er en nyttig ferdighet å ha i programmering, spesielt når vi jobber med tekstbehandling.

## Slik gjør du det

For å fjerne karakterer som matcher et mønster i Fish Shell, kan vi bruke kommandoen `string delete`, etterfulgt av mønsteret vi ønsker å slette og teksten eller filen den skal brukes på. For eksempel:

```
Fish Shell> string delete [mønster] [tekst/fil]
```

La oss si at vi har en tekstfil med tall og vi ønsker å fjerne alle tall fra den. Vi kan da bruke følgende kommando i Fish Shell:

```
Fish Shell> string delete [0-9] tall.txt
```

Dette vil fjerne alle tall fra filen `tall.txt` og gi oss en ren tekstfil uten tall.

## Dykk dypere

Det er flere måter å definere et mønster på når du bruker kommandoen `string delete`. For eksempel, hvis vi ønsker å fjerne alle tegn fra a til z i en tekststreng, kan vi bruke følgende mønster: `[a-z]`. Vi kan også benytte oss av regex-uttrykk for å fjerne mer komplekse mønstre.

En annen nyttig funksjon i Fish Shell er kommandoen `string replace`, som lar oss velge hva vi ønsker å erstatte et mønster med i teksten eller filen vår. Dette kan være nyttig hvis vi ønsker å endre et mønster til en bestemt verdi.

## Se også

- Fish Shell dokumentasjon: https://fishshell.com/docs/current/
- Regex tutorial: https://regexone.com/