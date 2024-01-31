---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:12:57.992837-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Att Hämta Nuvarande Datum i Bash

## Vad & Varför?
Att hämta det aktuella datumet innebär att du får fram dagens datum på din dator. Programmerare gör detta för att logga händelser, tidsstämpla filer eller för att hantera schemaläggning och deadlines.

## Hur gör man:
```Bash
# Visar bara datumet
date +"%Y-%m-%d"
```
```bash
# Exempelutskrift
2023-04-05
```

```Bash
# Visar datum och tid
date +"%Y-%m-%d %H:%M:%S"
```
```bash
# Exempelutskrift
2023-04-05 15:21:34
```

```Bash
# Sätter en variabel till dagens datum
TODAYS_DATE=$(date +"%Y-%m-%d")
echo $TODAYS_DATE
```
```bash
# Exempelutskrift
2023-04-05
```

## Djupdykning
Kommandot `date` i Bash har varit standard för Unix-liknande system sedan urminnes tider för att hämta tidsdata. Alternativ till `date` inkluderar att använda andra kommandon som `awk` eller att skriva egna skript i högre programmeringsspråk som Python eller Perl. Implementationen av `date` använder systemklockan och tidszonskonfigurationen för att ge korrekt datum och tid.

## Se också
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Datumformateringsguide: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Tidszoner och `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
