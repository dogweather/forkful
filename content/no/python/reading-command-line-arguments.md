---
title:    "Python: Lese kommandolinjeargumenter"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å lese kommandolinje-argumenter? Vel, det å kunne håndtere disse argumentene i koden din kan gjøre programmet ditt mer fleksibelt og tilpasningsdyktig. Ved å lese argumenter fra kommandolinjen kan du endre oppførselen til programmet ditt uten å måtte endre selve koden. Dette kan være nyttig når du ønsker å kjøre programmet ditt med forskjellige innstillinger eller behandle forskjellige datasett.

## Hvordan

For å lese kommandolinje-argumenter i Python, kan du bruke `sys`-modulen. Denne modulen gir tilgang til kommandolinje-argumentene som er gitt når programmet blir kjørt. La oss se på et eksempel:

```Python
import sys

# Les inn kommandolinje-argumenter
args = sys.argv[1:]

# Skriv ut alle argumentene
print("Kommandolinje-argumenter:")
for arg in args:
    print(arg)

```

Hvis du for eksempel kjører dette programmet med følgende kommandolinje `python program.py arg1 arg2 arg3`, vil output være:

```
Kommandolinje-argumenter:
arg1
arg2
arg3
```

Som du kan se, så blir kommandolinje-argumentene lagret i listen `args`. Deretter kan du behandle argumentene videre akkurat som du ville gjort med hvilken som helst annen liste i Python.

## Dypdykk

Vi kan også spesifisere argumenter med forskjellige flagg og argumentverdier. Dette kan være nyttig når du ønsker å angi bestemte innstillinger for programmet ditt. La oss ta en titt på et eksempel på hvordan du kan lese disse argumentene:

```Python
import sys

# Les inn kommandolinje-argumenter
# Første argument er flagget "-f" og dens verdi
# Andre argument er flagget "-n" og dens verdi
args = sys.argv[1:]

# Skriv ut filnavnet og antall linjer
print("Filnavn:", args[0])
print("Antall linjer:", args[1])

```

Hvis du nå kjører programmet med følgende kommandolinje `python program.py -f fil.txt -n 10`, vil output være:

```
Filnavn: fil.txt
Antall linjer: 10
```

Som du kan se, er det viktig å holde styr på rekkefølgen til argumentene når du leser dem. Du kan også spesifisere flere flagg og verdier for å tilpasse programmet ditt enda mer.

## Se også

- [Python dokumentasjon om sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Artikkel om lesing av kommandolinje-argumenter i Python](https://realpython.com/command-line-interfaces-python-argparse/)

Med disse tipsene kan du enkelt lese og håndtere kommandolinje-argumenter i dine Python-programmer. Lykke til med kodingen!