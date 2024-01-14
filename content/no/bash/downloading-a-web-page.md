---
title:                "Bash: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er interessert i å lære å kode, eller allerede er en erfaren programmerer, kan det å laste ned nettsider og manipulere data fra dem være en nyttig ferdighet. Ved å bruke Bash-programmeringsspråket, kan du enkelt laste ned og behandle informasjon fra hvilken som helst nettside. Dette kan være nyttig for å samle data, automatisere oppgaver eller opprette egne tilpassede verktøy.

## Hvordan

Her er et eksempel på hvordan du kan laste ned en nettside og hente ut spesifikk informasjon fra den ved hjelp av Bash-kode:

```Bash
#!/bin/bash

# Angi URL-adressen til nettsiden du ønsker å laste ned
url="https://www.example.com"

# Bruk wget-kommandoen for å laste ned nettsiden som en fil
wget -O webpage.html "$url"

# Bruk grep-kommandoen for å filtrere ut ønsket informasjon fra filen
# I dette eksempelet, henter vi ut alle linker fra nettsiden
grep -o '<a[^>]*>' webpage.html | grep -o 'href=[^>]*' | grep -o '".*"' | sed 's/"//g'

```

Koden over vil laste ned nettsiden og trekke ut alle linker som er nevnt på siden. Du kan også tilpasse koden til å hente ut annen informasjon, som for eksempel tekst, bilder eller tabeller.

## Dypdykk

For å forstå bedre hvordan denne koden fungerer, kan vi dykke litt dypere inn i de ulike kommandoene.

Først bruker vi `wget`-kommandoen for å laste ned nettsiden fra den angitte URL-adressen. `wget` er et populært verktøy for å laste ned filer fra internett i Bash. Neste kommando bruker så `grep` for å filtrere ut ønsket informasjon fra filen vi nettopp lastet ned. Med `grep -o`, vil vi bare få ut det som matcher det spesifiserte mønsteret, i dette tilfellet `<a href = "">`. Så bruker vi `sed` for å fjerne anførselstegnene rundt linkene som vi får ut.

Det er mange muligheter for å tilpasse denne koden og hente ut ulik informasjon fra nettsiden. Det kan også være lurt å eksperimentere med ulike kommandoer og søke etter ressurser og dokumentasjon for å lære mer om Bash-programmering og nettsidehenting.

## Se også

Her er noen nyttige ressurser og dokumentasjon du kan utforske for å lære mer om Bash-programmering og nettsidehenting:

- [Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for Nybegynnere](http://tille.garrels.be/training/bash/)
- [Wget dokumentasjon](https://www.gnu.org/software/wget/manual/wget.html)
- [Grep dokumentasjon](https://www.gnu.org/software/grep/manual/grep.html)