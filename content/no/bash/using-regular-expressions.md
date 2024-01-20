---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk er mønstre som brukes for å matche kombinasjoner av tegn i tekst. Programmerere bruker det fordi det gir dem muligheten til å søke, erstatte, og manipulere tekst mer effektivt.

## Hvordan

Enkelte eksempler på hvordan man kan bruke regulære uttrykk i Bash:

```Bash
# Sjekke om et ord finnes i en tekststring
echo "Hei verden" | grep -o "verden"

# Erstatte et ord i en tekststring
echo "Hei verden" | sed 's/verden/alle/'

# Sjekke om en tekststring matcher et bestemt mønster
[[ "abc123" =~ [a-zA-Z]+[0-9]+ ]] && echo "match!"
```
Utskriften vil være henholdsvis: "verden", "Hei alle", og "match!".

## Dypdykk

Regulære uttrykk oppsto først på 50-tallet og har siden blitt et essensielt verktøy i alle programmerere sin verktøykasse. Det finnes mange alternativer til regulære uttrykk som for eksempel "wildcards" og "globbing", men ingen gir den samme fleksibiliteten. Når du bruker regulære uttrykk i Bash er det viktig å bruke dem i kontekst av grep, sed, eller lignende kommandoer som forstår og kan tolke dem.

## Se også

For mer informasjon om regulære uttrykk: 
- [Regulære uttrykk i Bash guide](https://www.gnu.org/software/bash/manual/html_node/Regular-Expressions.html)
- [GEDP online regex tester](https://regex101.com/)
- [Stack Overflow tråder om regulære uttrykk](https://stackoverflow.com/questions/tagged/regex).