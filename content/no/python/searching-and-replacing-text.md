---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstatting av tekst er en måte å lokalisere og endre bestemte segmenter av strenger i koden. Programmerere gjør dette for å manipulere data effektivt, feilsøke, og rationalisere kode.

## Hvordan:
Søking og erstatting tekst i Python kan oppnås ved hjelp av `replace()` -metoden. Her er et enkelt eksempel:

```Python
tekst = "Jeg elsker eplepai"
endret_tekst = tekst.replace("eplepai", "blåbærpai")
print(endret_tekst)
```

Output:

```Python
Jeg elsker blåbærpai
```

I dette tilfellet er "eplepai" erstattet med "blåbærpai" i strengen.

## Dyp dykk:
Historisk sett har det å søke og erstatte tekst vært en kritisk funksjonalitet i redigering og databehandling. Det finnes mange metoder for å søke og erstatte tekst, inkludert bruk av regulære uttrykk, som kan gi mer fleksibilitet.

I Python blir `replace()` -metoden implementert i strengklassebiblioteket. Hvis ikke ny tekst er gitt, vil `replace()` ganske enkelt fjerne alle forekomster av målteksten.

Alternativt kan du bruke `re` -modulen for mer komplekse søk og erstattinger. Denne modulen tillater bruk av regulære uttrykk.

```Python
import re
tekst = "Jeg elsker eplepai og eplejuice"
endret_tekst = re.sub("eple", "blåbær", tekst)
print(endret_tekst)
```

Her erstatter `re.sub` alle forekomster av "eple" med "blåbær".

## Se også:
1. Python dokumentasjon - Strengmetoder: https://docs.python.org/3/library/stdtypes.html#string-methods
2. Python dokumentasjon - 're' modul: https://docs.python.org/3/library/re.html
3. En grundig veiledning for bruk av regulære uttrykk i Python: https://realpython.com/regex-python/
4. Søk og erstatt tekst i Python: https://www.geeksforgeeks.org/python-string-replace/