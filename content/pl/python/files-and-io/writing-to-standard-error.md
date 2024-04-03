---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:19.351279-07:00
description: "Jak to zrobi\u0107: #."
lastmod: '2024-03-13T22:44:34.967044-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Pisanie do standardowego b\u0142\u0119du"
weight: 25
---

## Jak to zrobić:


### Korzystając z `sys.stderr`
Wbudowany moduł `sys` w Pythonie pozwala na jawne pisanie do `stderr`. To podejście jest proste dla prostych komunikatów o błędach lub diagnostyki.

```python
import sys

sys.stderr.write('Błąd: Coś poszło nie tak.\n')
```
Przykładowe wyjście (do stderr):
```
Błąd: Coś poszło nie tak.
```

### Korzystając z funkcji `print`
Funkcja `print` w Pythonie może przekierować swoje wyjście do `stderr`, określając parametr `file`. Ta metoda jest przydatna do wykorzystania przyjazności funkcji `print` podczas obsługi komunikatów o błędach.
```python
from sys import stderr

print('Błąd: Awaria w module.', file=stderr)
```
Przykładowe wyjście (do stderr):
```
Błąd: Awaria w module.
```

### Korzystając z modułu `logging`
Dla bardziej kompleksowego rozwiązania, moduł `logging` w Pythonie może kierować komunikaty do `stderr` i wiele więcej, takie jak zapisywanie do pliku czy dostosowywanie formatu wiadomości. Ta metoda jest najlepsza dla aplikacji wymagających różnych poziomów logowania, formatowania wiadomości lub celów.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Błąd: Nie udało się połączyć z bazą danych.')
```
Przykładowe wyjście (do stderr):
```
ERROR:__main__:Błąd: Nie udało się połączyć z bazą danych.
```

### Biblioteki stron trzecich: `loguru`
`loguru` to popularna biblioteka stron trzecich, która upraszcza logowanie w aplikacjach Pythona. Automatycznie kieruje błędy do `stderr`, wśród innych funkcjonalności.

Aby użyć `loguru`, najpierw zainstaluj ją za pomocą pip:
```shell
pip install loguru
```

Następnie włącz ją do swojego skryptu Pythona w następujący sposób:
```python
from loguru import logger

logger.error('Błąd: Nie udało się otworzyć pliku.')
```
Przykładowe wyjście (do stderr):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Błąd: Nie udało się otworzyć pliku.
```
