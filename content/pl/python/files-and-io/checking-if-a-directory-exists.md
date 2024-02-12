---
title:                "Sprawdzanie, czy katalog istnieje"
aliases: - /pl/python/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:15.755911-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje w Pythonie, polega na weryfikacji obecności folderu w systemie plików przed wykonaniem operacji takich jak czytanie czy zapisywanie plików. Programiści robią to, aby uniknąć błędów takich jak `FileNotFoundError`, zapewniając, że aplikacja działa niezawodnie i nie zawiesza się podczas próby interakcji z katalogami.

## Jak to zrobić:
Python dostarcza natywne sposoby na sprawdzanie istnienia katalogu, wykorzystując moduły `os` i `pathlib`. Oto przykłady dla obu:

### Używając modułu `os`
```python
import os

# Podaj ścieżkę do katalogu
dir_path = "/ścieżka/do/katalogu"

# Sprawdź, czy katalog istnieje
if os.path.isdir(dir_path):
    print(f"Katalog {dir_path} istnieje.")
else:
    print(f"Katalog {dir_path} nie istnieje.")
```

### Używając modułu `pathlib`
```python
from pathlib import Path

# Podaj ścieżkę do katalogu
dir_path = Path("/ścieżka/do/katalogu")

# Sprawdź, czy katalog istnieje
if dir_path.is_dir():
    print(f"Katalog {dir_path} istnieje.")
else:
    print(f"Katalog {dir_path} nie istnieje.")
```

### Biblioteki firm trzecich
Chociaż standardowa biblioteka Pythona jest wystarczająca do sprawdzania, czy katalog istnieje, biblioteki takie jak `pathlib2` mogą być alternatywą dla zachowania spójności między wersjami Pythona lub dodatkowej funkcjonalności.

***Uwaga:*** W najnowszych wersjach Pythona `pathlib` jest wystarczająco solidny dla większości przypadków użycia, czyniąc biblioteki firm trzecich mniej potrzebnymi dla tego konkretnego zadania.
