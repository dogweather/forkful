---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive ut debug-informasjon handler om å kjøre ekstra kode i programmet ditt for å vise informasjon som hjelper deg med feilsøking. Programmerere gjør dette for lettere å forstå hva som fungerer, eller ikke fungerer, i programmet.

## Slik gjør du

Her er et enkelt eksempel på hvordan du skriver ut debug-informasjon i Python:

```python
def funksjon(x):
    print(f"Fungerer med {x}")
    resultat = x + 2
    print(f"Resultat: {resultat}")
    return resultat

funksjon(2)
```
Dette programmet vil produsere følgende utskrift:

```
Fungerer med 2
Resultat: 4
```

## Dypdykk

Historisk sett har skriving av debug-informasjon vært essensielt i programmeringsverdenen. Dette har gjort det mulig for utviklere å forstå detaljer og iterere over løsningene sine.

Som et alternativ til `print`-funksjonen, kan du bruke logging-biblioteket i Python. Dette biblioteket gir mer fleksibilitet og kontroll ved å ha forskjellige loggnivåer (INFO, DEBUG, ERROR, osv.), og lar deg skrive loggene til en fil i stedet for konsollen.

```python
import logging

logging.basicConfig(level=logging.DEBUG)
logging.debug('Dette vil bli printet')
logging.info('Så vel som dette')
logging.warning('Og dette')
```

I Python, kan du også bruke innebygde debugger verktøy som Pdb for interaktiv feilsøking, noe som kan tilby mer detaljert innsikt enn vanlig utskrift av debug-informasjon.

## Se Også

For mer detaljert informasjon og mer avanserte eksempler, ta en titt på disse ressursene:

1. [Python Documentation: Logging Module](https://docs.python.org/3/library/logging.html)
2. [Python Documentation: Pdb — The Python Debugger](https://docs.python.org/3/library/pdb.html)
3. [Real Python: A Guide to Python’s Print() Function](https://realpython.com/python-print/)