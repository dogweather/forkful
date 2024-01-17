---
title:                "Stor bokstav i en tekststreng"
html_title:           "Bash: Stor bokstav i en tekststreng"
simple_title:         "Stor bokstav i en tekststreng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr rett og slett å gjøre den første bokstaven stor, og alle de andre bokstavene små. Dette er vanligvis gjort for å gjøre en streng mer leselig og visuelt tiltalende.

Mange programmerere velger å kapitalisere strenger for å følge bestemte stilguider eller konvensjoner. Det kan også være nyttig når du jobber med data som skal sorteres eller sammenlignes.

## Hvordan:
Kodingseksempel: 
```
#!/bin/bash
str="dette er en teststreng"
echo "${str^}"
```
Utskrift:
```
Dette er en teststreng
```

En annen metode er å bruke "tr" kommandoen:
```
#!/bin/bash
str="dette er en teststreng"
echo $str | tr '[:lower:]' '[:upper:]'
```
Utskrift:
```
DETTE ER EN TESTSTRENG
```
## Dypdykk:
Historisk kontekst:
Kapitalisering av strenger kan spores tilbake til gamle dager med manuell skriving, der det var vanlig å gjøre alle bokstavene i en tittel eller overskrift store for å få det til å se mer formelt ut.

Alternativer:
I tillegg til "tr" kommandoen og strengmanipulasjon med "echo", kan du også bruke "sed" kommandoen for å kapitalisere en streng. Det finnes også spesialverktøy som er dedikert til å formatere strenger, som "rev" og "awk".

Implementeringsdetaljer:
I Bash, er det en rekke innebygde variabler som du kan bruke til å manipulere en streng. For eksempel, ${str^} vil kapitalisere den første bokstaven i "str", mens ${str^^} vil kapitalisere alle bokstavene.

## Se også:
- [Bash Manual](https://www.gnu.org/software/bash/manual/)
- [tr kommandoen](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [sed kommandoen](https://www.gnu.org/software/sed/manual/sed.html)
- [rev verktøyet](https://www.gnu.org/software/coreutils/manual/html_node/rev-invocation.html)
- [awk verktøyet](https://www.gnu.org/software/gawk/manual/gawk.html)