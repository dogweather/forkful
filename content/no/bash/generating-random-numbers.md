---
title:    "Bash: Generering av tilfeldige tall"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en nyttig ferdighet å ha i Bash-programmering, da det lar deg skape variasjon og tilfeldighet i dine skript. Dette er spesielt nyttig for å lage simuleringer, spill eller andre programmer som krever tilfeldige data.

## Hvordan

For å generere tilfeldige tall i Bash kan du bruke "RANDOM" kommandoen. Denne kommandoen vil gi deg et tilfeldig tall mellom 0 og 32767 hver gang den blir kjørt. For å generere et tilfeldig tall innenfor et bestemt område, kan du bruke modulus-operatøren (%).

```
Bash
#!/bin/bash
# Dette skriptet genererer et tilfeldig tall mellom 1 og 10
RANDOM_NUMBER=$((RANDOM % 10 + 1))
echo "Det tilfeldige tallet er $RANDOM_NUMBER"
```

Dette vil gi deg tall mellom 1 og 10 hver gang du kjører skriptet. Du kan også bruke andre matematiske operatører, som for eksempel multiplikasjon eller divisjon, for å generere et tilfeldig tall innenfor et større område.

## Dypere dykk

Bash har også andre alternativer for å generere tilfeldige tall, som for eksempel "shuf" kommandoen. Denne kommandoen lar deg velge et tilfeldig element fra en gitt liste eller fil. For å generere et tilfeldig tall mellom to tall i en fil, kan du bruke følgende kommando:

```
Bash
shuf -i 1-100 -n 1
```

Dette vil gi deg et tilfeldig tall mellom 1 og 100. Du kan også kombinere dette med andre kommandoer for å utføre forskjellige handlinger basert på det tilfeldige tallet som blir generert.

## Se også

- Bash dokumentasjon for RANDOM: https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM
- Bash dokumentasjon for modulus-operatøren: https://www.gnu.org/software/bash/manual/html_node/Arithmetic-Expansion.html
- Bash dokumentasjon for shuf: https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html