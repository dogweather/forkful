---
date: 2024-01-20 17:57:00.657515-07:00
description: "How to: | Jak to zrobi\u0107: Po pierwsze, zaimportuj `argv` z modu\u0142\
  u `sys`. Potem mo\u017Cesz wykorzysta\u0107 `argv` do pobrania argument\xF3w."
lastmod: '2024-04-05T22:37:43.677618-06:00'
model: gpt-4-1106-preview
summary: "| Jak to zrobi\u0107: Po pierwsze, zaimportuj `argv` z modu\u0142u `sys`.\
  \ Potem mo\u017Cesz wykorzysta\u0107 `argv` do pobrania argument\xF3w."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to: | Jak to zrobić:
Po pierwsze, zaimportuj `argv` z modułu `sys`. Potem możesz wykorzystać `argv` do pobrania argumentów.

```Python
import sys

# Wydrukuj wszystkie argumenty linii poleceń
print(f'Wszystkie argumenty: {sys.argv}')

# Pobierz konkretny argument, np. pierwszy po nazwie skryptu
if len(sys.argv) > 1:
    first_arg = sys.argv[1]
    print(f'Pierwszy argument: {first_arg}')
else:
    print('Nie podano argumentów oprócz nazwy skryptu.')

# Uruchom skrypt z linii poleceń:
# python skrypt.py arg1 arg2
```

Możliwy output:
```
Wszystkie argumenty: ['skrypt.py', 'arg1', 'arg2']
Pierwszy argument: arg1
```

## Deep Dive | Szczegółowe zagłębienie:
Historia argumentów linii poleceń sięga czasów przed graficznymi interfejsami użytkownika - to staromodny, ale niezawodny sposób na komunikację z programami. Alternatywą dla `sys.argv` jest użycie modułów takich jak `argparse` do bardziej zaawansowanego parsowania argumentów, z możliwością dodania opisów, typów wartości, i domyślnych wartości.

Implementacja `sys.argv` jest prosta - `argv[0]` to zawsze nazwa skryptu, a `argv[1]` to pierwszy argument (jeśli istnieje), oraz tak dalej. Pamiętaj, że argumenty są zawsze odczytywane jako łańcuchy znaków, więc często potrzebujesz konwersji na inne typy danych.

## See Also | Zobacz również:
- Oficjalna dokumentacja Pythona dla `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- Tutorial `argparse`: https://docs.python.org/3/howto/argparse.html
- Inne metody służące do parsowania argumentów linii poleceń, jak `getopt`: https://docs.python.org/3/library/getopt.html
