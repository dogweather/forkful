---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Elm: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy dany katalog istnieje w Pythonie, to po prostu sposób na upewnienie się, czy katalog, z którym chcemy pracować, faktycznie istnieje w systemie plików. Programiści robią to, aby uniknąć błędów podczas próby pracy z nieistniejącym katalogiem.

## Jak to zrobić:

Możemy to zrobić za pomocą modułu os. Oto proste przykłady:

```Python
import os

# Sprawdzamy, czy katalog istnieje
if os.path.isdir("/ścieżka/do/katalogu"):
    print("Katalog istnieje")
else:
    print("Katalog nie istnieje")
```

Jeśli chcemy sprawdzić i stworzyć katalog, jeśli go nie ma, możemy użyć:

```Python
directory = "/ścieżka/do/katalogu"

if not os.path.exists(directory):
    os.makedirs(directory)
```

## Deep Dive: 

Sprawdzanie, czy katalog istnieje, to stara praktyka znana wszystkim programistom pracującym z systemami plików. W Pythonie możemy to zrobić na kilka sposobów, ale najpopularniejsze i najbardziej "pythoniczne" są metody `os.path.isdir` i `os.path.exists` wraz z `os.makedirs`.

Warto zauważyć, że `os.path.exists` sprawdza, czy dana ścieżka istnieje, niezależnie od tego, czy jest to katalog, czy plik. Jeśli potrzebujesz sprawdzić specyficznie katalog, powinieneś użyć `os.path.isdir`.

Alternatywą dla modułu os jest moduł pathlib, który jest częścią standardowej biblioteki od Pythona 3.4.

```Python
from pathlib import Path

# Sprawdzenie, czy katalog istnieje
if Path('/ścieżka/do/katalogu').is_dir():
    print('Katalog istnieje.')
else:
    print('Katalog nie istnieje.')
```

## Zobacz również: 

- Dokumentacja Pythona na temat modułów os and pathlib: https://docs.python.org/3/library/os.path.html, https://docs.python.org/3/library/pathlib.html
- Artykuł — Praca z plikami i katalogami w Pythonie: https://realpython.com/working-with-files-in-python/
- Post na StackOverflow na temat sprawdzania istnienia katalogu: https://stackoverflow.com/questions/8933237/how-to-find-if-directory-exists-in-python