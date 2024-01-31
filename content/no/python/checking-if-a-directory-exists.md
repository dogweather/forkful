---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:58:16.833807-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe finnes i Python betyr å verifisere at en bestemt sti peker til en reell mappe. Programmerere trenger å vite dette for å unngå feil under filhåndtering, som for eksempel ved lesing, skriving eller sletting av filer.

## Hvordan gjøre det:
```Python
import os

# Sti til mappen du vil sjekke
mappe_sti = "/eksempel/mappe"

# Sjekk om mappen finnes
if os.path.isdir(mappe_sti):
    print(f"Mappen '{mappe_sti}' finnes.")
else:
    print(f"Mappen '{mappe_sti}' finnes ikke.")
```

### Sample Output
```
Mappen '/eksempel/mappe' finnes ikke.
```

## Dypdykk
Før i tiden brukte man ofte `os.path.exists()` for å sjekke om en fil eller mappe fantes. Problemet er at `exists` kan være misvisende siden den returnerer `True` for både filer og mapper. Derfor er `os.path.isdir()` mer pålitelig når man eksplisitt vil vite om en sti peker til en mappe. Et alternativ til `os`-modulen er å bruke `pathlib`, et bibliotek introdusert i Python 3.4, som gir et høyere abstraksjonsnivå for filsystemoperasjoner.

```Python
from pathlib import Path

# Sti til mappen du vil sjekke
mappe_sti = Path("/eksempel/mappe")

# Sjekk om mappen finnes bruker Path.exists()
if mappe_sti.is_dir():
    print(f"Mappen '{mappe_sti}' finnes.")
else:
    print(f"Mappen '{mappe_sti}' finnes ikke.")
```

Bare husk at `is_dir()` vil gi `False` hvis stien ikke eksisterer overhodet, så kombinere det med `exists()` kan være nyttig for å skille mellom ikke-eksisterende stier og stier som peker til filer.

## Se Også
- Python's offisielle dokumentasjon for `os`-modulen: https://docs.python.org/3/library/os.html
- Python's offisielle dokumentasjon for `pathlib`-modulen: https://docs.python.org/3/library/pathlib.html
- En nyttig Stack Overflow diskusjon om emnet: https://stackoverflow.com/questions/8933237/how-to-find-if-directory-exists-in-python
