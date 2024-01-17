---
title:                "Søke og erstatte tekst"
html_title:           "Python: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstattelse av tekst er en vanlig programmeringsoppgave. Det handler om å finne og erstatte en bestemt del av en tekst med en annen del. Dette kan være nyttig når man ønsker å gjøre endringer i store tekstfiler eller når man ønsker å erstatte gammel kode med ny kode.

## Hvordan:
For å søke og erstatte tekst i Python kan du bruke innebygde funksjoner som `replace()` og `sub()`. Se eksempler nedenfor for å lære hvordan du kan bruke disse funksjonene i praksis.

```
# Eksempel 1: Erstatt et ord i en setning
setning = "Jeg elsker å programmere."
ny_setning = setning.replace("programmere", "kode")
print(ny_setning)
# Output: Jeg elsker å kode.

# Eksempel 2: Erstatt flere ord i en tekst
tekst = "Epler, appelsiner, bananer."
ny_tekst = re.sub("Epler|appelsiner", "jordbær", tekst)
print(ny_tekst)
# Output: Jordbær, jordbær, bananer.
```

## Dypdykk:
Søking og erstatting av tekst har vært en viktig del av programmering siden begynnelsen. Tidligere ble dette gjort manuelt, men i dag har vi avanserte verktøy og funksjoner som gjør det mye enklere. Et alternativ til å bruke innebygde funksjoner i Python er å bruke eksterne moduler som `re` som er spesialisert på å søke og erstatte tekst. Det er også viktig å være oppmerksom på forskjellige metoder og regex-uttrykk som kan brukes for å gjøre søking og erstatting mer effektivt.

## Se også:
For videre lesning og utforsking av søking og erstatting i Python, kan du sjekke ut følgende ressurser:

- Python dokumentasjon om `replace()` og `sub()`: https://docs.python.org/3/library/stdtypes.html#str.replace https://docs.python.org/3/library/re.html#re.sub
- En guide til regular expressions i Python: https://realpython.com/regex-python/
- En tutorial om å søke og erstatte tekst i Python: https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial