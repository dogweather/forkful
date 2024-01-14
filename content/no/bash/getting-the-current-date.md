---
title:                "Bash: Hente nåværende dato"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få dagens dato er en vanlig oppgave i Bash-programmering. Dette kan være nyttig for å lage skript som automatiserer oppgaver som kjører på bestemte datoer, som for eksempel å ta sikkerhetskopier av filer, sende statusrapporter eller planlegge automatiserte prosesser. 

## Hvordan

For å få dagens dato i Bash kan du bruke `date`-kommandoen. Dette vil skrive ut dagens dato i standardformatet som er satt på systemet ditt, vanligvis i form av måned/dag/år. Her er et eksempel på hvordan du kan bruke `date` i en Bash-skript:

```Bash
#!/bin/bash

today=$(date)
echo "Dagens dato er $today"
```

Dette vil skrive ut følgende når det blir kjørt:

```
Dagens dato er Mon May 24 15:10:03 CEST 2021
```

Du kan også kontrollere hvilket format datoen blir skrevet ut i ved å bruke `date`-kommandoen med forskjellige opsjoner som `+%d/%m/%Y` for å få formatet dag/måned/år. Her er en eksempel på hvordan du kan bruke dette i en Bash-skript:

```Bash
#!/bin/bash

today=$(date +%d/%m/%Y)
echo "Dagens dato er $today"
```

Dette vil skrive ut følgende når det blir kjørt:

```
Dagens dato er 24/05/2021
```

Du kan også bruke `date`-kommandoen til å få dagens dato i en annen tidssone ved å bruke opsjonen `-d` etterfulgt av ønsket tidssone. For eksempel, `date -d "America/New_York"` vil skrive ut dagens dato i New York.

## Dypdykk

Hvis du vil lære mer om hvordan `date`-kommandoen fungerer, kan du lese manualsidene ved å skrive `man date` i terminalen. Her kan du finne alle de forskjellige opsjonene og formatene som er tilgjengelige for `date`-kommandoen. Du kan også bruke `--help`-opsjonen for å få en kort oversikt over de vanligste opsjonene.

## Se også

- [Bash How To: Automate Tasks with Cron Jobs](https://www.makeuseof.com/tag/cron-linux-job-scheduler/)
- [Bash scripting cheat sheet](https://devhints.io/bash)
- [Bash manual pages](https://www.gnu.org/software/bash/manual/)