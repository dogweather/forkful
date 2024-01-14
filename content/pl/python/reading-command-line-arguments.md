---
title:                "Python: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w języku Python, musimy mieć możliwość wprowadzania różnych danych bezpośrednio z wiersza poleceń. W tym celu wykorzystujemy argumenty wiersza poleceń, które pozwalają nam na wprowadzenie danych przy uruchamianiu programu. W artykule tym dowiesz się, dlaczego jest to przydatne i jak to zrobić.

## Jak To Zrobić

Aby użyc argumentów wiersza poleceń w Pythonie, musimy skorzystać z modułu `sys` oraz metody `argv`. Poniższy kod przedstawia przykładową funkcję, która wyświetla argumenty wprowadzone z wiersza poleceń oraz ich ilość:

```Python
import sys

def print_arguments():
    arguments = sys.argv
    number_of_args = len(arguments)
    print("Wprowadzone argumenty: " + str(arguments))
    print("Ilość argumentów: " + str(number_of_args))

if __name__ == "__main__":
    print_arguments()
```

Gdy uruchomimy ten program z przypisanymi argumentami, np. `python arguments.py arg1 arg2`, otrzymamy następujący output:

```
Wprowadzone argumenty: ['arguments.py', 'arg1', 'arg2']
Ilość argumentów: 3
```

Jak widzimy, argumenty są przechowywane w liście `sys.argv` a ich ilość możemy sprawdzić za pomocą funkcji `len()`. Możemy również odczytać pojedynczy argument, odnosząc się do jego indeksu w liście. Np. `sys.argv[1]` zwróci wartość `"arg1"`.

## Deep Dive

Wraz z argumentami wiersza poleceń, możemy również wykorzystać flagi, które pozwalają nam na przekazanie dodatkowych informacji do naszego programu. Flagi są zwykle wprowadzane w postaci `-flag_wartość` lub `--flag=wartość`. W Pythonie do odczytania flag możemy wykorzystać moduł `argparse`, który daje nam większą kontrolę nad tym, jakie informacje chcemy przekazać do naszego programu. Poniższy kod przedstawia przykładowe użycie modułu `argparse`:

```Python
import argparse

def print_args():
    parser = argparse.ArgumentParser()
    
    parser.add_argument("-n", "--name", help="Wprowadź imię")
    parser.add_argument("-a", "--age", type=int, help="Wprowadź wiek")

    args = parser.parse_args()

    print("Witaj " + args.name + "!")
    if args.age:
        print("Masz " + str(args.age) + " lat.")

if __name__ == "__main__":
    print_args()
```

Gdy uruchomimy ten program z flagami, np. `python args.py -n Jan -a 30`, otrzymamy następujący output:

```
Witaj Jan!
Masz 30 lat.
```

Jak widzimy, możemy przekazać wartości dla określonych flag, a także wykorzystać `help` aby wyświetlić informacje o danym argumencie. Dzięki temu nasz program staje się bardziej czytelny dla użytkownika.

## Zobacz również

* [Dokumentacja modułu sys w języku Python](https://docs.python.org/3/library/sys.html)
* [Dokumentacja modułu argparse w języku Python](https://docs.python.org/3/library/argparse.html)