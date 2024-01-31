---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:58:17.994546-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i Dlaczego?"

Sprawdzanie, czy katalog istnieje, to sposób na uniknięcie błędów podczas próby dostępu do nieistniejących folderów. Robimy to, aby nasz kod był niezawodny i bezpiecznie obsługiwał pliki i foldery.

## How to:
"Jak to zrobić:"

Sprawdzamy istnienie katalogu w Pythonie z modułem `os` lub `pathlib`.

```python
import os

# Używając os.path
if os.path.isdir("/ścieżka/do/katalogu"):
    print("Katalog istnieje.")
else:
    print("Katalog nie istnieje.")

# Używając pathlib
from pathlib import Path

if Path("/ścieżka/do/katalogu").is_dir():
    print("Katalog istnieje.")
else:
    print("Katalog nie istnieje.")
```
Wyjście zależy od tego, czy katalog istnieje:
```
Katalog istnieje.
```
Lub:
```
Katalog nie istnieje.
```

## Deep Dive:
"Dogłębne Zanurzenie:"

Metoda `os.path.isdir()` istnieje w Pythonie od dawna. `pathlib`, dodana w Python 3.4, jest nowocześniejszą alternatywą oferującą obiektowo zorientowany interfejs. W przeciwieństwie do `os.path`, `pathlib` może także efektywnie łączyć ścieżki.

Używanie `os` jest typowe dla starszych programów, ale `pathlib` przynosi większą czytelność i jest często rekomendowany jako bardziej "pythoniczny".

## See Also:
"Zobacz Również:"

- Dokumentacja modułu `os`: https://docs.python.org/3/library/os.html
- Dokumentacja modułu `pathlib`: https://docs.python.org/3/library/pathlib.html
- Porównanie os.path i pathlib: https://treyhunner.com/2018/12/why-you-should-be-using-pathlib/
