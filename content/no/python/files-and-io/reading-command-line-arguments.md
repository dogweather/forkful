---
date: 2024-01-20 17:56:49.590353-07:00
description: "Kommandolinjeargumenter lar brukere gi inndata direkte til et program\
  \ n\xE5r de kj\xF8rer det fra en terminal. Programmerere henter disse argumentene\
  \ for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.376639-06:00'
model: gpt-4-1106-preview
summary: "Kommandolinjeargumenter lar brukere gi inndata direkte til et program n\xE5\
  r de kj\xF8rer det fra en terminal."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hvordan:
```Python
import sys

def hoved():
    if len(sys.argv) > 1:
        for i, arg in enumerate(sys.argv[1:], start=1):
            print(f"Argument {i}: {arg}")
    else:
        print("Ingen argumenter ble gitt.")

if __name__ == "__main__":
    hoved()
```

**Eksempel på kjøring:**

```bash
$ python mitt_skript.py hei verden
Argument 1: hei
Argument 2: verden
```

## Dypdykk:
Å lese kommandolinjeargumenter er en praksis så gammel som kommandolinjen selv. Det dateres tilbake til tider med terminal-baserte operativsystemer. Python bruker `sys.argv` for å lagre argumentene, hvor `sys.argv[0]` er skriptnavnet. Alternativer inkluderer bruk av `argparse` modulen for mer kompleksitet og `click` pakken for moderne CLI applikasjoner. `sys.argv` er rå og enkelt, men kraftig for små skript eller når enkelheten er nøkkelen.

## Se Også:
- Python-dokumentasjon for `sys` modulen: https://docs.python.org/3/library/sys.html
- `argparse` modulen: https://docs.python.org/3/library/argparse.html
- `click` dokumentasjon: https://click.palletsprojects.com/en/7.x/
