---
title:    "Fish Shell: Utvinning av understrenger"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å hente ut en del av en tekststreng, men visste ikke hvordan? Da er å ekstrahere substrings noe som kan være nyttig for deg! Det lar deg enkelt og raskt hente ut deler av en tekststreng basert på bestemte kriterier.

## Hvordan

For å ekstrahere substrings i Fish Shell, bruker vi kommandoen "string" etterfulgt av et kolon (:) og deretter et tall eller en bokstav som indikerer hvilken del av tekststrengen du ønsker å hente ut. La oss si for eksempel at vi har en tekststreng, "Hei, jeg er en Fish bruker", og vi ønsker å hente ut "Fish" fri for teksten. Her er et eksempel på hvordan du kan gjøre dette:

```Fish Shell
set sentence "Hei, jeg er en Fish bruker"
echo $sentence:string condition
```

Dette vil gi oss følgende utskrift:

```
Fish
```

Vi brukte "string" for å indikere at vi ønsker å hente ut en del av $setningen, og deretter fulgte vi med "condition" som er den delen vi ønsker å ekstrahere.

Vi kan også bruke en tallverdi etter kolon, for eksempel hvis vi ønsker å hente ut de første fire bokstavene i teksten:

```Fish Shell
echo $sentence:string 0 4
```

Output vil være:

```
Hei,
```

## Dypdykk

Hvis du ønsker å ekstrahere en substring basert på et annet mønster enn en tallverdi eller bokstav, kan du bruke "contains" kommandoen. Denne lar deg hente ut en del av tekststrengen som inneholder et bestemt ord eller uttrykk.

La oss si at vi har en tekststreng, "Jeg elsker å spise sushi", og vi ønsker å hente ut "sushi". Her er et eksempel på hvordan vi kan gjøre dette:

```Fish Shell
set sentence "Jeg elsker å spise sushi"
echo $sentence:contains sushi
```

Dette vil gi oss følgende output:

```
sushi
```

Vi brukte "contains" for å indikere at vi ønsker å hente ut en del av $setning som inneholder ordet "sushi".

## Se Også

- Fish Shell sine offisielle dokumentasjon om substrings: https://fishshell.com/docs/current/cmds/string.html
- En tutorial om hvordan å ekstrahere substrings i Fish Shell: https://www.fosslinux.com/38506/how-to-extract-substrings-from-string-in-fish-shell.htm
- En Stack Overflow-tråd om ekstrahering av substrings: https://stackoverflow.com/questions/39295678/how-to-get-first-letter-of-string-in-fish-shell