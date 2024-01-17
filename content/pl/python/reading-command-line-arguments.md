---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Python: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Co to jest i dlaczego to robimy?

Czy kiedykolwiek zastanawiałeś się, jak programy na twoim komputerze otrzymują informacje z wiersza poleceń? To właśnie odczytywanie argumentów z linii poleceń jest odpowiedzialne za tę funkcjonalność. Programiści często korzystają z niego, aby móc dostosować swoje programy do konkretnych potrzeb użytkownika.

# Jak to zrobić?

Odczytywanie argumentów z linii poleceń w Pythonie jest bardzo proste. Wystarczy użyć specjalnej funkcji sys.argv, która jest dostępna w standardowej bibliotece języka. Wystarczy zaimportować bibliotekę sys i użyć jej funkcji argv, aby otrzymać listę wszystkich argumentów przekazanych do programu. Na przykład:

```python
import sys

# Przykładowy program, który odczytuje i wyświetla argumenty z linii poleceń
print("Liczba przekazanych argumentów:", len(sys.argv))
print("Argumenty z linii poleceń:", sys.argv)
```

Przykładowe wywołanie tego programu mogłoby wyglądać następująco:

```
> python program.py hello world
Liczba przekazanych argumentów: 3
Argumenty z linii poleceń: ['program.py', 'hello', 'world']
```

# Głębsza analiza

Odczytywanie argumentów z linii poleceń nie jest nową funkcjonalnością w programowaniu. Już w latach 70-tych, kiedy powstał język C, zaimplementowano to rozwiązanie. Alternatywą może być użycie biblioteki argparse, która oferuje większą kontrolę nad przetwarzaniem argumentów oraz dodatkowe funkcje takie jak generowanie pomocy dla użytkownika. 

Implementacja odczytywania argumentów z linii poleceń w Pythonie jest bardzo intuicyjna i nie wymaga dużo kodu. Warto jednak pamiętać o różnych wyjątkach, które mogą wystąpić, np. brak przekazanych argumentów lub błędny format. W takich przypadkach, warto zastosować odpowiednie bloki try-catch, aby wyłapać i obsłużyć te wyjątki.

# Zobacz także

Więcej informacji na temat odczytywania argumentów z linii poleceń w Pythonie można znaleźć w oficjalnej dokumentacji języka: https://docs.python.org/3/library/sys.html#sys.argv 

Dla bardziej zaawansowanych funkcjonalności związanych z obsługą argumentów z linii poleceń, warto zapoznać się z biblioteką argparse: https://docs.python.org/3/library/argparse.html