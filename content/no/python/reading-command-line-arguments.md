---
title:    "Python: Lesing av kommandolinjeargumenter"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor
Å lese kommandolinje-argumenter er en viktig del av å skrive effektiv Python-kode. Det gjør det mulig å gi brukeren mer kontroll og fleksibilitet når de bruker programmet ditt, og det kan også gjøre det enklere å automatisere deler av koden din.

## Hvordan
For å lese kommandolinje-argumenter i Python, bruker du et innebygd bibliotek kalt "argparse". Først må du importere dette biblioteket i koden din:

```Python
import argparse
```

Deretter kan du definere hvilke kommandolinje-argumenter du vil lese inn ved hjelp av ArgumentParser-objektet:

```Python
parser = argparse.ArgumentParser()
parser.add_argument("--navn", help="Navnet ditt")
parser.add_argument("--alder", type=int, help="Alderen din")
args = parser.parse_args()
```

I dette eksempelet definerer vi to kommandolinje-argumenter: "--navn" og "--alder". Vi setter også et "help" -argument som vil bli brukt til å gi en beskrivelse av argumentet når brukeren spør om det.

Når du har definert argumentene dine, kan du bruke dem i koden din som variabler:

```Python
print("Hei " + args.navn + ", du er " + str(args.alder) + " år gammel.")
```

Når du kjører koden din fra kommandolinjen og gir med argumentene "--navn" og "--alder", vil du få følgende output:

```bash
$ python min_kode.py --navn Jonas --alder 25
Hei Jonas, du er 25 år gammel.
```

## Deep Dive
For å lese enda mer kompliserte kommandolinje-argumenter, kan du bruke "nargs" -argumentet til ArgumentParser-objektet. Dette lar deg definere antall verdier som kan leses fra ett enkelt argument. Du kan også bruke "choices" -argumentet til å begrense mulige verdier for et argument.

Det finnes også mange flere funksjoner og metoder i "argparse" -biblioteket som kan hjelpe deg med å håndtere kommandolinje-argumenter på en mer avansert måte. Utforsk dokumentasjonen for å lære mer om disse.

## Se også
- [Dokumentasjon for "argparse" biblioteket](https://docs.python.org/3/library/argparse.html)
- [En grundig tutorial om å lese og behandle kommandolinje-argumenter i Python](https://realpython.com/command-line-interfaces-python-argparse/)
- [En praktisk bruk av "argparse" for å bygge et kommandolinje-verktøy](https://scotch.io/tutorials/build-a-command-line-application-with-python)