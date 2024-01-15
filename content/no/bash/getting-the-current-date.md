---
title:                "Å få nåværende dato"
html_title:           "Bash: Å få nåværende dato"
simple_title:         "Å få nåværende dato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle man ønske å få gjeldende dato? Vel, det kan være flere grunner til det. Kanskje du vil lagre datoen som en del av en filnavn eller for å automatisk generere rapporter med datoangivelser.

## Slik gjør du det
For å få gjeldende dato i Bash, kan du bruke kommandoen `date`. La oss se på noen eksempler på hvordan du kan bruke denne kommandoen:

```Bash
# Få gjeldende dato og tid i standardformat
$ date
Fri 25 Jun 2021 12:34:56 UTC

# Få kun gjeldende dato i formatet mm/dd/åååå
$ date +%m/%d/%Y
06/25/2021

# Få kun gjeldende år
$ date +%Y
2021
```

Som du kan se, kan du bruke `+%` etterfulgt av et spesifikt format for å få ønsket informasjon fra datoen. Du kan også bruke kommandoen `man date` for å se en liste over alle tilgjengelige formater.

## Dypdykk
I tillegg til standardformater, kan du også bruke `date`-kommandoen til å få dato og tid i en hvilken som helst tidszone, samt å justere dato og tid. Du kan også konvertere datoen til et annet format ved hjelp av variabler og funksjoner.

For eksempel, for å få dato og tid i Oslo tidszone, kan du bruke `TZ`-variabelen som følger:

```Bash
$ TZ="Europe/Oslo" date
Fri 25 Jun 2021 14:34:56 CEST
```

For å justere datoen med en spesifikk tidsperiode, kan du bruke `date -d`-kommandoen. For eksempel, for å få datoen én uke fra nå, kan du gjøre følgende:

```Bash
$ date -d "1 week"
Fri 2 Jul 2021 12:00:00 UTC
```

Det er også mulig å konvertere datoen til sekunder siden 1. januar 1970, også kjent som Unix-tiden. Dette kan gjøres ved hjelp av `-u`-flagget.

```Bash
$ date -u +"%s"
1624612476
```

Det finnes mange flere måter å tilpasse og bruke `date`-kommandoen på. Det er verdt å utforske denne kommandoen nærmere for å få mest mulig ut av den.

## Se også
* [Bash dokumentasjon for `date`-kommandoen](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
* [Bash-dato og klokkeslett-manualen](https://tldp.org/LDP/abs/html/timedate.html)
* [Linuxman-siden for `date`-kommandoen](https://linux.die.net/man/1/date)