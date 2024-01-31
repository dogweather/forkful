---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:34:37.900031-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en streng innebærer å konvertere tekst til et datoobjekt. Vi gjør dette for å kunne manipulere og sammenligne datoer, og for å integrere data i systemer som forventer spesifikke datoformater.

## Hvordan:
Her er et enkelt Bash-script som parser en dato fra en streng ved hjelp av `date`-kommandoen.

```Bash
#!/bin/bash

date_string="21-03-2023"
parsed_date=$(date -d $date_string '+%Y-%m-%d')

echo "Parsed date: $parsed_date"
```

Når du kjører dette skriptet, vil du få følgende utdata:

```
Parsed date: 2023-03-21
```

## Dypdykk:
Parsing av dato i programmering går tilbake til de tidlige dagene av informatikk da man måtte standarisere data. Alternativer til `date`-kommandoen i Bash inkluderer å bruke programmingsspråk som Python eller Perl med kraftigere datoparsing-biblioteker. I Bash, når man implementerer datoparsing, må man være oppmerksom på locale-innstillinger og tidsstandarder, for eksempel forskjellen mellom MM-DD-YYYY og DD-MM-YYYY formatene.

## Se også:
- `man date` for å få mer informasjon om `date`-kommandoen.
- [GNU Coreutils - Date documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) for inngående detaljer om `date`.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) for en total oversikt over Bash-scripting, inkludert datohåndtering.
