---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:15.523886-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente den gjeldende datoen i programmering betyr å få tak i dagens dato og tidspunkt. Programmere gjør dette for å loggføre hendelser, håndtere tidsavhengige funksjoner eller bare for å vise datoen.

## Slik gjør du:
```Bash
# For å få den gjeldende datoen og tiden i standardformat:
date

# For å tilpasse formatet, bruk "+format":
date +"%Y-%m-%d"
date +"%H:%M:%S"

# Eksempel på standardformat:
# fre. 07 april 2023 13:37:22 +0200

# Eksempel med tilpasset format (bare dato):
# 2023-04-07

# Eksempel med tilpasset format (bare klokkeslett):
# 13:37:22
```

## Dypdykk:
`date`-kommandoen har vært en essensiell del av Unix og Linux-systemer siden de tidlige dagene. Alternativer som `hwclock` kan også brukes for å hente tid direkte fra maskinvaren, selv om `date` er mer vanlig. På implementasjonsnivå henter `date` verdier fra systemklokken som oppdateres av operativsystemet i henhold til tidssonen som er satt av brukeren.

I skript og programmer kan du for eksempel bruke `date` til å generere tidsstempel for logger, time jobber med `cron` eller for å stille inn dato- og tidskrav i applikasjoner. Det er også mulig å sette systemtiden med `date`, men det krever administratorrettigheter.

## Se også:
- [Date Man Pages](https://man7.org/linux/man-pages/man1/date.1.html) - manualen for `date` med flere detaljer og eksempler.
- [Bash Scripting Guide](https://www.tldp.org/LDP/abs/html/) - en guide til skripting i Bash, inkludert bruk av dato og tid.
- [Advanced Bash-Scripting Guide: Date and Time](https://www.tldp.org/LDP/abs/html/timedate.html) - dybdeinformasjon om hvordan man håndterer dato og tid i Bash-skript.
