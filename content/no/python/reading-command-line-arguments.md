---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Å lese kommandolinjeargumenter er å hente informasjon fra brukerinngang gitt i terminalen når et python-program kjøres. Programmerere gjør dette for å motta spesifikke parametere eller instruksjoner ved kjøretid.
 
## Hvordan gjør man det:
Bruken av det innebygde sys-modulen i Python er den vanligste måten å hente kommandolinjeargumenter. Her er et eksempel:

```Python
import sys

def main():
    # Viser alle argumenter. Argument 0 er alltid filnavn
    print('Alle argumenter:', sys.argv)

    # Viser det første argumentet (etter filnavnet)
    if len(sys.argv) > 1:
        print('Første argument:', sys.argv[1])

if __name__ == "__main__":
    main()
```
Når du kjører programmet med argumenter, vises disse argumentene som følger:

```Shell
> python3 kommando.py parameter1 parameter2
Alle argumenter: ['kommando.py', 'parameter1', 'parameter2']
Første argument: parameter1
```

## Detaljert dykk:
- Historisk kontekst: Bruken av kommandolinjeargumenter strekker seg tilbake til de tidlige dagene av kodemerking. Dette ga brukere mulighet til å gi input ved kjøretid snarere enn å hardkode verdier.
- Alternativer: argparse-modulen er en annen vanlig metode for å behandle kommandolinjeargumenter. Den gir mer robuste verktøy for parsing av argumenter og generering av hjelpetekster.
- Implementasjonsdetaljer: sys.argv er en liste i Python, som inneholder alle kommandolinjeargumentene sendt til scriptet. Det zeroende elementet, sys.argv[0], er alltid navnet på scriptet selv.

## Se også:
- [Python sys Modul Dokumentasjon](https://docs.python.org/3/library/sys.html)
- [Python argparse Modul Dokumentasjon](https://docs.python.org/3/library/argparse.html)
- [En grundigere dykkning inn i å behandle kommandolinjeargumenter i Python](https://realpython.com/command-line-interfaces-python-argparse/)