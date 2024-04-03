---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.755911-07:00
description: "Jak to zrobi\u0107: Python dostarcza natywne sposoby na sprawdzanie\
  \ istnienia katalogu, wykorzystuj\u0105c modu\u0142y `os` i `pathlib`. Oto przyk\u0142\
  ady dla obu: #."
lastmod: '2024-03-13T22:44:34.964756-06:00'
model: gpt-4-0125-preview
summary: "Python dostarcza natywne sposoby na sprawdzanie istnienia katalogu, wykorzystuj\u0105\
  c modu\u0142y `os` i `pathlib`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

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
