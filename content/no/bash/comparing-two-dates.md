---
title:                "Bash: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger i programmering må vi sammenligne to datoer. Dette kan være nyttig for å sjekke om et arrangement allerede har skjedd, eller om en bestemt hendelse er planlagt å skje i fremtiden. Ved å sammenligne datoer kan vi enkelt automatisere prosesser og unngå manuelle beregninger.

## Hvordan
Det er flere måter å programmere sammenligning av datoer på, men i dag vil jeg vise deg hvordan du kan gjøre det i Bash. Først må vi bruke kommandoen `date` for å få datoen i ønsket format. La oss si at vi vil sammenligne dagens dato med en spesifikk dato, for eksempel 12. desember 2020.

```
#!/bin/bash

# Få dagens dato i formatet dd.mm.yyyy
today=$(date +"%d.%m.%Y")

# Definer en annen dato
other_date="12.12.2020"

# Sammenlign dagens dato med den andre datoen
if [ "$today" = "$other_date" ]
then
  echo "Dette er samme dato!"
else
  echo "Datoene er forskjellige."
fi

```

Dette eksemplet brukes også for å vise hvordan du kan bruke en `if`-setning i Bash. Når du kjører dette skriptet, vil utgangen være "Dette er samme dato!" hvis dagens dato er 12. desember 2020, ellers vil utgangen være "Datoene er forskjellige.".

## Dypdykk
Når du sammenligner datoer, er det viktig å huske på at de må være i samme format for at sammenligningen skal fungere. For eksempel, hvis du prøver å sammenligne "12-12-2020" med "12.12.2020", vil dette ikke fungere siden de har forskjellig format.

En annen ting å merke seg er at Bash ikke har innebygde funksjoner for å sammenligne datoer, så dette må implementeres gjennom variasjoner av `date`-kommandoen og bruk av `if`-setninger.

## Se Også
- [Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Date kommandoen](https://www.tutorialspoint.com/unix_commands/date.htm)
- [If-setninger i Bash](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-6.html)