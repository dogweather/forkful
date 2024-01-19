---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Co i dlaczego?
Czytanie argumentów z linii poleceń to proces, w którym program odczytuje dane wprowadzone przez użytkownika podczas uruchamiania programu. Programiści robią to, aby umożliwić użytkownikom dostosowywanie działania programu bez konieczności jego modyfikowania.

# Jak to zrobić:
Python ma wbudowany moduł `sys` do odczytywania argumentów z linii poleceń. Po prostu zaimportuj ten moduł i skorzystaj z listy `sys.argv`. Pierwszym elementem listy jest nazwa programu i są tu zapisane wszystkie argumenty linii komend.

```Python 
import sys

for i in range(len(sys.argv)):
    print(f"Argument {i}: {sys.argv[i]}")
```

Wywołanie powyższego programu z argumentami linii poleceń `arg1 arg2 arg3` da wynik:

```Bash
Argument 0: script.py
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

# Przyjrzyjmy się bliżej
Czytanie argumentów z linii poleceń to stary trik w programowaniu. Zostało to zaprojektowane dla systemów operacyjnych typu Unix i zostało przyjęte przez wiele języków programowania.

Istnieją alternatywne metody do odczytu argumentów z linii poleceń w Pythonie, jak `argparse` lub bibliotekę `click`. `argparse` jest modaulem wbudowanym i oferuje wiele opcji konfiguracyjnych, podczas gdy `click` jest zewnętrzną biblioteką, która jest łatwa do użycia i czytelna.

Szczegółem implementacyjnym jest to, że `sys.argv` nie zawiera wartości domyślnych dla argumentów, które nie zostały podane przy uruchomieniu programu. Musisz samodzielnie zapewnić logiczną obsługę takich przypadków.

# Zobacz też
1. Moduł `sys` w dokumentacji Pythona: https://docs.python.org/3/library/sys.html
2. Moduł `argparse` w dokumentacji Pythona: https://docs.python.org/3/library/argparse.html
3. Biblioteka `click`: https://click.palletsprojects.com/
4. Arguments command line w Python - tutorial: https://realpython.com/python-command-line-arguments/