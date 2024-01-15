---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Python: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan gjøre programmene dine mer interaktive? Vil du vite hvordan du kan endre oppførselen til et program uten å endre selve koden? Da bør du lære deg hvordan du kan lese og håndtere kommandolinje-argumenter i Python!

## Slik gjør du det

Det første du må gjøre er å importere sys-biblioteket i Python. Dette biblioteket gir tilgang til systemspesifikke funksjoner, blant annet kommandolinje-argumenter.

```Python
import sys
```

For å lese argumentene som er gitt når du kjører et program i kommandolinjen, kan du bruke sys.argv-listen. Denne listen inneholder alle argumentene, der det første argumentet er navnet på programmet.

La oss si at du kjører programmet ditt slik: `python program.py argument1 argument2`

Da vil sys.argv-listen inneholde følgende:

```
['program.py', 'argument1', 'argument2']
```

Du kan nå hente ut og bruke disse argumentene i koden din. Her er et eksempel på hvordan du kan skrive ut argumentene du har gitt til programmet:

```Python
import sys

print("Argumentene du ga til programmet er:")
print(sys.argv[1:])
```

Output for dette eksemplet vil være:

```
Argumentene du ga til programmet er:
['argument1', 'argument2']
```

## Dypdykk

Hvis du ønsker å gå dypere inn i dette emnet, kan du lese mer om hvordan du håndterer kommandolinje-argumenter i Python i dokumentasjonen til sys-modulen. Du kan også lære mer om hvordan du kan gi argumenter til et Python-program ved hjelp av argumentparser-modulen.

## Se også

- [Dokumentasjon for sys-modulen](https://docs.python.org/3/library/sys.html)
- [Håndtering av kommandolinje-argumenter i Python](https://realpython.com/command-line-interfaces-python-argparse/)
- [Argumentparser-modulen](https://docs.python.org/3/library/argparse.html)