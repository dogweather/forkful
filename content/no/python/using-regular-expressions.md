---
title:                "Python: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Python-programmerer eller en som er interessert i å lære å kode, har du kanskje hørt om noe som heter "regular expressions". Dette er et kraftig verktøy som lar deg søke etter og manipulere tekststrenger på en svært effektiv måte. Enten du trenger å analysere store mengder tekst eller forenkle en komplisert oppgave, kan regular expressions være den perfekte løsningen.

## Slik gjør du det

For å bruke regular expressions i Python, må du først importere "re"-modulen. Deretter kan du bruke ulike funksjoner som denne modulen tilbyr for å utføre ulike søk og manipulasjoner på tekststrenger. La oss se på noen eksempler:

```Python
import re

# Søke etter en bestemt streng
re.search(r'god', "God morgen!").group() # Output: god

# Søke etter tall
re.findall(r'\d+', "Det er 2020").group() # Output: 2020

# Erstatte deler av en streng
re.sub(r'hallo', 'hei', "Hallo, hvordan går det?") # Output: Hei, hvordan går det?
```

Som du kan se, bruker vi en spesiell syntaks for å definere mønstre som vi vil søke etter. Dette kan være enkeltbokstaver, tall, spesielle karakterer eller en kombinasjon av disse. Ved å bruke ulike metodekall kan vi få tilbake ønsket resultat.

## Dykk dypere

Regular expressions kan virke litt overveldende i begynnelsen, men når du blir vant til det, vil du sette pris på hvor nyttig det kan være. Det finnes et stort antall metoder som kan brukes for å søke og manipulere tekststrenger, samt ulike syntaksregler for å definere mønstre. Hvis du ønsker å lære mer om dette, anbefaler jeg å sjekke ut [Official Python Documentation](https://docs.python.org/3/library/re.html) eller [Regular-Expressions.info](https://www.regular-expressions.info/) for mer detaljert informasjon.

## Se også

- [Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/) - En nyttig ressurs for å lære de grunnleggende syntaksreglene for regular expressions.
- [Regex101](https://regex101.com/) - En online regex tester hvor du kan eksperimentere med ulike uttrykk og få matchende resultater.
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/2e/chapter7/) - En praktisk guide for å lære å bruke regular expressions i Python, med konkrete eksempler og prosjekter.