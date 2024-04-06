---
date: 2024-01-20 17:56:49.590353-07:00
description: "Hvordan: **Eksempel p\xE5 kj\xF8ring:**."
lastmod: '2024-04-05T21:53:41.354725-06:00'
model: gpt-4-1106-preview
summary: "**Eksempel p\xE5 kj\xF8ring:**."
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
