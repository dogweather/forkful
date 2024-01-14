---
title:                "Python: Lese kommandolinje-argumenter"
simple_title:         "Lese kommandolinje-argumenter"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er en viktig ferdighet for alle som ønsker å bli effektive Python-programmerere. Det gir deg muligheten til å interagere med programmene dine på en mer dynamisk måte og gjør det enklere å håndtere store og komplekse datasett.

## Hvordan

Prosessen med å lese kommandolinje-argumenter i Python er enkel og kan gjøres med bare noen få linjer med kode. Først må du importere sys-modulen, som lar deg få tilgang til argumentene som er gitt ved kjøring av programmet. Deretter kan du bruke funksjonen "sys.argv" for å få en liste over alle argumentene. Her er et eksempel på hvordan du kan lese et enkelt argument og skrive det ut:

```Python
import sys

navn = sys.argv[1]

print("Hei " + navn)
```

I eksempelet over bruker vi sys.argv[1] for å få tilgang til det første argumentet (indeksert som 1 i stedet for 0). Når du kjører programmet og gir et navn som argument, vil det bli skrevet ut en hilsen med navnet ditt.

## Dypdykk

For de som ønsker å lære mer om håndtering av kommandolinje-argumenter i Python, er det verdt å merke seg at du også kan bruke modulen "argparse". Dette gjør det mulig å legge til mer funksjonalitet, som å legge til flagg og parameterverdier for å tilpasse programmet ditt ytterligere.

Når du bruker argparse, må du definere de ulike argumentene og deres tilhørende aksjoner. Du kan også legge til beskrivelser og standardverdier for hvert argument. Her er et eksempel på hvordan du kan bruke argparse for å lese to argumenter og utføre en enkel matematisk operasjon:

```Python
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("tall1", type=int, help="Første tall")
parser.add_argument("tall2", type=int, help="Andre tall")

args = parser.parse_args()

resultat = args.tall1 + args.tall2

print("Resultat: " + str(resultat))
```

Når du kjører programmet og gir to tall som argumenter, vil resultatet av summen av tallene bli skrevet ut.

## Se også

- Python's offisielle dokumentasjon om kommandolinje-argumenter: [https://docs.python.org/3/library/sys.html](https://docs.python.org/3/library/sys.html)
- Tutorial om bruk av argparse: [https://realpython.com/command-line-interfaces-python-argparse/](https://realpython.com/command-line-interfaces-python-argparse/)
- Eksempelkode og mer informasjon om hvordan du kan bruke kommandolinje-argumenter i Python: [https://www.tutorialspoint.com/python/python_command_line_arguments.htm](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)