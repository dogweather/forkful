---
title:                "Python: Odczytywanie parametrów linii poleceń"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak przełączyć opcje w swoim programie bez edytowania kodu? W tym blogu dowiesz się, dlaczego warto i jak możesz zacząć czytać argumenty linii poleceń w Pythonie.

## Jak to zrobić

Zaczniemy od najprostszego przykładu, gdzie wprowadzimy tylko jeden argument z linii poleceń. Używając funkcji sys.argv, możemy odczytać argumenty, które zostaną przekazane do programu. Przykład:

```Python
import sys 

print("Wprowadziłeś/aś argument: ", sys.argv[1])
```

Teraz, jeśli uruchomisz ten kod ze specyficznym argumentem (np. "python program.py argument"), program wyświetli wiadomość "Wprowadziłeś/aś argument: argument".

Możesz także wprowadzić więcej niż jeden argument. Wtedy każdy kolejny argument będzie przechowywany w osobnej komórce sys.argv. Przykład:

```Python
import sys 

print("Wprowadziłeś/aś pierwszy argument: ", sys.argv[1])
print("Wprowadziłeś/aś drugi argument: ", sys.argv[2])
```

Teraz, jeśli uruchomisz ten kod ze specyficznymi argumentami (np. "python program.py argument1 argument2"), program wyświetli wiadomość "Wprowadziłeś/aś pierwszy argument: argument1" oraz "Wprowadziłeś/aś drugi argument: argument2".

## Deep Dive

Funkcja sys.argv jest użyteczna, jednak warto zwrócić uwagę na kilka ważnych rzeczy. Po pierwsze, pierwszym argumentem w sys.argv jest nazwa pliku z kodem. Dlatego pierwszy argument, który wprowadzisz, będzie przechowywany w sys.argv[1].

Po drugie, argumenty przekazywane do programu są przechowywane jako ciągi znaków (stringi). Dlatego warto konwertować je na odpowiedni typ danych, jeśli chcesz wykonać działania matematyczne lub inne operacje na nich.

## Zobacz także

- Dokumentacja Pythona na temat sys.argv: https://docs.python.org/3/library/sys.html
- Wideo tutorial na YouTube "Python Command Line Arguments": https://youtu.be/niPBw_7DebM
- Przykładowy kod na GitHubie: https://github.com/python-examples/learn-python/blob/master/read-command-line-arguments.py