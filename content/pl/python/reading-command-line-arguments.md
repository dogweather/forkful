---
title:    "Python: Odczytywanie argumentów wiersza poleceń"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego programy Python mogą przyjąć argumenty z wiersza poleceń? Może wydaje ci się to mało pomocne lub bezużyteczne. Jednak jest to jeden z najbardziej przydatnych i wszechstronnych sposobów na wchodzenie w interakcję ze swoim programem. Dzięki argumentom wiersza poleceń możesz zmienić zachowanie programu bez konieczności zmiany kodu. W tym artykule dowiesz się, dlaczego warto poznać jak czytać argumenty z wiersza poleceń w Pythonie.

## Jak to zrobić

Jest kilka sposobów na czytanie argumentów z wiersza poleceń w Pythonie. Najprostszym z nich jest użycie modułu `sys`. Poniższy kod pokazuje, jak można wyświetlić wszystkie argumenty przekazane do programu:

```Python
import sys

print(sys.argv)
```
Wywołanie powyższego kodu z argumentami `python program.py argument1 argument2` da nam następujący wynik:

```
['program.py', 'argument1', 'argument2']
```

Widać tutaj, że pierwszym elementem listy jest nazwa programu. Następnie mamy wszystkie przekazane argumenty.

Możesz również przekazać argumenty jako opcje z użyciem modułu `argparse`. Poniżej znajduje się przykładowy kod, który wyświetli przesłane argumenty oraz wartość opcji `--type`:

```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('arguments', nargs='*')
parser.add_argument('--type')

args = parser.parse_args()

print(args.arguments)
print(args.type)
```

Wywołanie powyższego kodu z argumentami `python program.py argument1 argument2 --type string` da nam następujący wynik:
```
['argument1', 'argument2']
string
```

Jak widać, możemy wygodnie dostosować kod przy użyciu opcji i opcjonalnych argumentów.

## Deep Dive

Warto dokładniej zapoznać się z modułem `argparse` i wszystkimi możliwymi opcjami. Pozwala on na obsługę wielu różnych rodzajów argumentów i umożliwia dokładniejsze kontrolowanie programu.

Ponadto, w razie potrzeby możesz użyć również innych modułów, takich jak `click` czy `docopt`, aby czytać argumenty z wiersza poleceń. Ważne jest, aby dobrać najlepsze rozwiązanie do konkretnego programu i jego potrzeb.

## Zobacz też

- [Dokumentacja modułu `sys`](https://docs.python.org/3/library/sys.html)
- [Dokumentacja modułu `argparse`](https://docs.python.org/3/library/argparse.html)
- [Dokumentacja modułu `click`](https://click.palletsprojects.com/en/7.x/)
- [Dokumentacja modułu `docopt`](https://github.com/docopt/docopt)