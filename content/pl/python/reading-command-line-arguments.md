---
title:    "Python: Odczytywanie argumentów linii poleceń"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie programowania musimy dostarczyć dodatkowe informacje do naszego skryptu. Może to być na przykład ścieżka do pliku czy opcje konfiguracyjne. W takich przypadkach bardzo przydatne jest czytanie argumentów z linii poleceń. W tym artykule dowiesz się jak to zrobić w języku Python.

## Jak to zrobić

```Python
import sys

#Pobieramy argumenty linii poleceń
argumenty = sys.argv

#Iterujemy przez argumenty i wyświetlamy je
for arg in argumenty:
  print(arg)
```

Przykładowe wywołanie naszego skryptu z linii poleceń: ```python skrypt.py argument1 argument2```

W takim przypadku otrzymamy na wyjściu:

```
skrypt.py
argument1
argument2
```

Możemy także określić, które argumenty chcemy pominąć i zacząć iterację od konkretnego elementu:

```Python
import sys

#Pobieramy argumenty, zaczynamy od drugiego elementu (pierwszym jest nazwa skryptu)
argumenty = sys.argv[1:]

#Iterujemy przez argumenty i wyświetlamy je
for arg in argumenty:
  print(arg)
```

W takim przypadku, jeśli wywołamy nasz skrypt tak: ```python skrypt.py argument1 argument2 argument3```

Otrzymamy na wyjściu:

```
argument1
argument2
argument3
```

## Pogłębiona analiza

Czy już wiesz, że możemy także przekazywać argumenty do naszego skryptu w postaci flag i wartości? Może to być o wiele wygodniejsze niż wpisywanie ich po prostu po kolei. Używając modułu ```argparse``` w naszym skrypcie możemy zdefiniować wszystkie dostępne opcje oraz opisy dla nich. Następnie, uruchamiając nasz skrypt z odpowiednim zestawem flag, automatycznie przypisane zostaną im odpowiednie wartości. To może usprawnić i uporządkować nasze skrypty.

## Zobacz także
- [Dokumentacja Pythona o przetwarzaniu argumentów z linii poleceń](https://docs.python.org/3/library/argparse.html)
- [Tutorial na temat modułu argparse](https://realpython.com/command-line-interfaces-python-argparse/)
- [Przykładowe użycie argparse w projekcie Django](https://docs.djangoproject.com/en/3.0/howto/custom-management-commands/#accepting-arguments-through-the-command-line)