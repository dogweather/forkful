---
title:                "Python: Sletting av tegn som matcher et mønster"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se nærmere på hvordan man kan slette tegn som matcher et spesifikt mønster i en tekststreng ved hjelp av Python-programmering. Dette kan være nyttig dersom man for eksempel ønsker å rense data eller formatere tekst på en mer effektiv måte.

## Hvordan gjøre det

Først må vi importere regular expression-modulen i Python, også kjent som "re". Dette vil tillate oss å jobbe med mønstre i tekststrenger. Deretter kan vi bruke funksjonen "sub" i "re"-modulen for å erstatte alle tegn som matcher mønsteret med en tom streng.

```Python
import re

tekst = "Hei! Hvorfor skal vi slette disse tegnene: @$%^&?"

ny_tekst = re.sub("[@$%^&?]", "", tekst)

print(ny_tekst)
```

Dette ville gi følgende utskrift:

```
Hei! Hvorfor skal vi slette disse tegnene: ?
```

I dette tilfellet brukte vi et enkelt tegnsett for å matche, men man kan også bruke mer kompliserte mønstre ved hjelp av regulære uttrykk. For eksempel kan man slette alle tall fra en tekststreng ved å bruke "[0-9]+" som mønster.

## Dypdykk

Regulære uttrykk kan være nyttige verktøy for å behandle og manipulere tekst på en mer effektiv måte. Ved å bruke metoder som "sub" og "findall" i "re"-modulen, kan man finne og erstatte ulike tegn eller ord i en tekststreng. Det finnes også en rekke spesifikke metoder som kan brukes til å søke etter bestemte mønstre basert på for eksempel bokstaver, tall eller spesialtegn.

## Se også

- [The Python re Module](https://docs.python.org/3/library/re.html)
- [Python Regular Expression Operations](https://www.w3schools.com/python/python_regex.asp)
- [RegExr](https://regexr.com/) - Nettside for å teste og lære om regulære uttrykk