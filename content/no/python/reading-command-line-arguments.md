---
title:                "Python: Lesing av kommandolinje-argumenter"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter er en viktig del av Python-programmering, spesielt når du ønsker å lage programmer som er mer interaktive og brukervennlige. Ved å lese kommandolinjeargumenter kan du gi brukere muligheten til å justere og tilpasse programmet ditt etter deres behov, noe som kan forbedre brukeropplevelsen og gjøre koden din mer robust.

## Hvordan

For å lese kommandolinjeargumenter i Python, kan du bruke `sys.argv` funksjonen. Denne funksjonen tar inn argumenter gitt ved å kjøre programmet fra kommandolinjen og lagrer dem i en liste. La oss se på et eksempel:

```Python
import sys

# Første element i sys.argv-listen er alltid navnet på programmet
print("Programnavn:", sys.argv[0])

# Du kan få tilgang til de andre argumentene ved å indeksere sys.argv-listen
print("Første argument:", sys.argv[1])
print("Andre argument:", sys.argv[2])
```

Hvis du for eksempel lagret denne koden i en fil ved navn `les-args.py`, og kjørte `python les-args.py Hei verden`, ville du få følgende utskrift:

```
Programnavn: les-args.py
Første argument: Hei
Andre argument: verden
```

Som du kan se, blir de argumentene du gir etter filnavnet lagret og kan brukes i koden din. Du kan også bruke `len(sys.argv)` for å finne ut hvor mange argumenter som ble gitt.

## Dypdykk

Det finnes også andre måter å lese kommandolinjeargumenter på, for eksempel ved å bruke biblioteker som `argparse` eller `click`. Disse gir mer avanserte muligheter for å lese og behandle argumenter, og er spesielt nyttige for større og mer komplekse programmer. Det er verdt å utforske disse bibliotekene hvis du ønsker å lære mer om hvordan du kan lese og behandle kommandolinjeargumenter på en mer avansert måte.

## Se også

- Dokumentasjon for `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- `argparse` biblioteket: https://docs.python.org/3/library/argparse.html
- `click` biblioteket: https://click.palletsprojects.com/en/8.0.x/