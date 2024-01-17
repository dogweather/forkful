---
title:                "Søke og erstatte tekster"
html_title:           "Bash: Søke og erstatte tekster"
simple_title:         "Søke og erstatte tekster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søking og erstatning av tekst er en vanlig oppgave for programmerere. Dette innebærer å finne et bestemt stykke tekst og erstatte det med en annen. Å gjøre dette kan hjelpe til med å endre eller forbedre en kode eller tekstfil. Det er en effektiv måte å automatisere endringer på og sikre konsistens i koden din.

## Hvordan:

Vi kan bruke "sed" kommandoen i Bash for søking og erstatning av tekst. Dette er et kraftig verktøy som lar oss søke etter et bestemt mønster i en fil og deretter erstatte det med en annen tekst eller en annen fil. La oss si at vi har en fil kalt "hello.txt" som inneholder følgende tekst:

```
Hello world! Dette er en test.
```

Hvis vi vil erstatte "Hello world" med "Hei verden" kan vi bruke følgende kommando:

```
sed -i 's/Hello world/Hei verden/g' hello.txt
```

Denne kommandoen vil søke gjennom filen, finne mønsteret "Hello world" og erstatte det med "Hei verden". "-i" -flagget lar oss gjøre endringene direkte i den opprinnelige filen. Merk at "s" betyr "substitute" (erstatte) og "g" betyr "global", dette betyr at alle forekomster av mønsteret vil bli erstattet.

## Dypdykk:

Søking og erstatning av tekst ble først introdusert i ed teksteditoren på 1970-tallet og har siden blitt en standardfunksjon i mange programmeringsspråk og verktøy. I tillegg til sed, kan vi bruke grep, awk, perl og andre språk for å utføre lignende oppgaver. Det er også verdt å nevne at det finnes grafiske programmer som kan utføre disse oppgavene på en mer brukervennlig måte. Hvis du vil gå mer i dybden på hvordan søking og erstatning fungerer, kan du lese om regulære uttrykk (regex) som er et kraftig verktøy for å finne og manipulere tekst.

## Se også:

- [Bash sed dokumentasjon](https://www.gnu.org/software/sed/manual/sed.html)
- [Introduksjon til regulære uttrykk](https://www.regular-expressions.info/)
- [10 grep-kommandoer alle bør kjenne til](https://www.linode.com/docs/tools-reference/tools/10-grep-command-examples/)