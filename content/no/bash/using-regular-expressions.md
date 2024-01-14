---
title:    "Bash: Å bruke regulære uttrykk"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor bruke regulære uttrykk?

Regulære uttrykk (også kjent som regex) er kraftige verktøy som brukes til å søke og manipulere tekststrenger. De kan være nyttige for å løse komplekse oppgaver innen bash-programmering, som å finne og erstatte data eller validere brukerinput. Å forstå hvordan man bruker regulære uttrykk kan gjøre det enklere å lage mer effektiv og pålitelig kode.

# Hvordan bruke regulære uttrykk i Bash

For å bruke regulære uttrykk i Bash, kan du bruke kommandoen "grep". Denne kommandoen tar et mønster og en fil som argumenter, og returnerer alle linjene som samsvarer med mønsteret. Her er et eksempel som vil finne alle linjer som inneholder ordet "hund" i filen "dyr.txt":

```bash
grep "hund" dyr.txt
```

Dette vil returnere alle linjene som inneholder ordet "hund" i filen "dyr.txt". Hvis du vil finne alle linjer som begynner med bokstaven "a", kan du bruke regex-mønsteret "^a". La oss se et eksempel på dette:

```bash
grep "^a" dyr.txt
```

Dette vil returnere alle linjer i filen som starter med bokstaven "a". Du kan også bruke regulære uttrykk til å erstatte tekst. For eksempel, hvis du ønsker å erstatte alle forekomster av ordet "katt" med "hund" i filen "dyr.txt", kan du bruke følgende kommando:

```bash
sed 's/katt/hund/g' dyr.txt
```

Dette vil erstatte alle forekomster av ordet "katt" med "hund" i filen og vise resultatet i terminalen.

# En dypere dykk i regulære uttrykk

Regulære uttrykk kan være komplekse, men de kan også være svært nyttige når man jobber med tekstbehandling. Du kan bruke ulike spesialtegn og operatorer for å lage mer presise mønstre. Her er noen eksempler:

- "." betyr hvilken som helst enkelt tegn
- "+" betyr at forrige tegn må forekomme minst én gang
- "*" betyr at forrige tegn kan forekomme null eller flere ganger
- "{n}" betyr at forrige tegn må forekomme n nøyaktig ganger
- "|" betyr "eller", som lar deg angi flere alternativer for et mønster
- "()" lar deg gruppere mønstre for å utføre operasjoner på dem

Det finnes også mange andre spesialtegn og operatorer som kan brukes til å lage avanserte regex-mønstre. Det kan være nyttig å eksperimentere og se hvordan forskjellige tegn påvirker søkeresultatene i ulike situasjoner.

# Se også

- [Bash Guide for nybegynnere](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Regex Tutorial av Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html)
- [Grep manuell side](https://linux.die.net/man/1/grep)