---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Go: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sjekke om en katalog eksisterer er en måte å se om en bestemt filsti eksisterer i systemet. Programmere gjør dette for å hindre feil når et program prøver å lese eller skrive til en ikke-eksisterende katalog.

## Hvordan gjøre det:

Bruk innebygde `os` og `os.path` biblioteker for sjekke om en katalog eksisterer. Her er et eksempel med utskrift.

```Python
import os

directory_path = "/path/to/the/directory"

if os.path.isdir(directory_path):
    print("Katalogen eksisterer")
else:
    print("Katalogen eksisterer ikke")
```
Hvis katalogen eksisterer vil programmet skrive ut "Katalogen eksisterer", ellers vil det skrive ut "Katalogen eksisterer ikke".

## Tief Dykk:

Det er nyttig å vite noen detaljer når vi bruker dette i virkelige koder. Historisk sett er kontrollere eksistensen av en katalog en viktig del av filbehandling i programmering. Dette prinsippet holder også i dag.

Et alternativ til `os.path.isdir()` metoden er bruk av `Path().exists()` i pathlib-modulen. Her er et eksempel:

```Python
from pathlib import Path

directory = Path("/path/to/the/directory")

if directory.exists():
    print("Katalogen eksisterer")
else:
    print("Katalogen eksisterer ikke")
```

Så hvordan fungerer metoden `os.path.isdir()`? Først henter det filsti som en streng, og deretter bruker det lavnivå operativ system kall for å sjekke om stien peker til en eksisterende katalog.

## Se Også:

For mer informasjon, sjekk ut følgende kilder:

1. [Offisiell Python-documentation for os-modulen](https://docs.python.org/3/library/os.html)
2. [Offisiell Python-documentation for pathlib-modulen](https://docs.python.org/3/library/pathlib.html)
3. [Stackoverflow post om forskjellene mellom os og pathlib](https://stackoverflow.com/questions/6004073/how-can-i-check-if-a-directory-exists)