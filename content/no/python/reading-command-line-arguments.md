---
title:                "Lese kommandolinjeargumenter"
html_title:           "Python: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å lese kommandolinje-argumenter er en essensiell del av programmering, da det tillater oss å kommunisere med programmene vi lager på en dynamisk måte. Vi kan gi våre programmer ulike parametere for å endre hvordan de fungerer uten å måtte endre selve koden.

# Hvordan:
```Python
import sys

# Kommando linje-argumenter kan leses fra sys.argv-listen
# Indeksen 0 vil alltid være selve programmet, så de faktiske argumentene starter fra indeks 1
argumenter = sys.argv[1:]

# Argumentene kan nå brukes i koden, for eksempel å skrive en velkomstmelding
navn = argumenter[0]
print("Hei " + navn + ", velkommen til mitt program!")
```

Output:
```
> python mittprog.py Anna
Hei Anna, velkommen til mitt program!
```

# Dypdykk:
Å lese kommandolinje-argumenter er ikke noe nytt konsept, og har vært en del av programmering siden tidlig på 1970-tallet. I tillegg til å bruke sys.argv, finnes det også alternativer som argparse-modulen som tillater mer avansert håndtering av argumenter. Det er også viktig å være oppmerksom på at kommandolinje-argumenter må håndteres riktig for å unngå sikkerhetshull i programmer.

# Se også:
- https://docs.python.org/3/library/sys.html#sys.argv
- https://docs.python.org/3/library/argparse.html
- https://www.geeksforgeeks.org/command-line-arguments-in-python/