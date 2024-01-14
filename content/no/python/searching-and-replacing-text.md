---
title:                "Python: Søking og erstatting av tekst"
simple_title:         "Søking og erstatting av tekst"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Å kunne søke og erstatte tekst er en viktig ferdighet for alle som ønsker å lære Python. Det sparer tid og gjør det enklere å redigere store mengder tekst på en effektiv måte.

## Hvordan
For å søke og erstatte tekst i Python, kan du bruke funksjonen `replace()`. Denne funksjonen tar to argumenter: det første er teksten du vil erstattet, og det andre er teksten du vil erstatte med. Her er et enkelt eksempel:

```Python
tekst = "Jeg elsker å programmere i Python"
ny_tekst = tekst.replace("elsker", "digge")
print(ny_tekst)
```

Output:

```
Jeg digger å programmere i Python
```

Du kan også bruke `replace()` for å bytte ut flere deler av en tekst i en og samme funksjon. Se på dette eksempelet:

```Python
tekst = "Jeg liker å lære nye programmeringsspråk, men Python vil alltid være favoritten min"
ny_tekst = tekst.replace("lik", "elsk").replace("vil", "er")
print(ny_tekst)
```

Output:

```
Jeg elsker å lære nye programmeringsspråk, men Python er alltid favoritten min
```

## Dypdykk
Når det gjelder søk og erstatting i Python, kan du også bruke regulære uttrykk. Dette er mer avansert, men gir større fleksibilitet når det kommer til søkemønstre. Du kan bruke `re`-modulen for å jobbe med regulære uttrykk i Python. Her er et eksempel som viser hvordan du kan bruke `re.sub()` for å erstatte deler av en tekst:

```Python
import re

tekst = "Jeg digger Python, er det ikke fantastisk?"
ny_tekst = re.sub(r"ikke ", "", tekst)
print(ny_tekst)
```

Output:

```
Jeg digger Python, er det fantastisk?
```

For å lære mer om regulære uttrykk og hvordan du kan bruke de i Python, kan du se på følgende ressurser:

- [Python sin offisielle dokumentasjon for `re`-modulen](https://docs.python.org/3/library/re.html)
- [Tutorial: Regular Expressions in Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)
- [Regular expressions cheat sheet for Python](https://www.debuggex.com/cheatsheet/regex/python)

## Se også
- [En innføring i å jobbe med tekst i Python](https://realpython.com/python-strings/)
- [10 måter å manipulere tekst i Python på](https://towardsdatascience.com/beyond-replace-manipulating-text-data-with-string-methods-in-python-559f4d9a0946)
- [En fullstendig guide til regulære uttrykk i Python](https://www.geeksforgeeks.org/regular-expression-python/)