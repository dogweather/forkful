---
title:                "Generowanie losowych liczb"
html_title:           "Python: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Czym jest generowanie liczb losowych i dlaczego programiści to robią?

Generowanie liczb losowych to proces, w którym komputer tworzy wartości liczbowe w sposób przypadkowy. Programiści często wykorzystują to w swoim kodzie, aby wprowadzić element przypadkowości lub symulować sytuacje, w których występują losowe wartości.

# Jak to zrobić?

Python zapewnia wiele wbudowanych funkcji do generowania liczb losowych. Aby ich użyć, musisz najpierw zaimportować moduł "random". Poniżej znajduje się przykładowy kod generujący losową liczbę całkowitą w zakresie od 1 do 10:

```python
import random

print(random.randint(1, 10))
```

Powyższy kod wywoła wyjście takie jak to:

```
7
```

Możesz również wygenerować losowe liczby zmiennoprzecinkowe przy użyciu funkcji "random()" oraz zdefiniować własny zakres. Przykładowy kod może wyglądać tak:

```python
import random

print(random.random() * 10) # wygeneruje losową liczbę zmiennoprzecinkową w zakresie od 0 do 10
```

# Głębsze zagadnienia

Pierwsza metoda generowania liczb losowych została opracowana przez amerykańskiego matematyka George'a Marsagliego w 1964 roku. Obecnie dostępne są również inne metody, takie jak metoda mieszana L'Ecuyera czy algorytm Mersenna Twister. Istnieją również biblioteki do wykonywania bardziej skomplikowanych operacji na liczbach losowych, takich jak ciągi Fibonacciego czy symulacje Monte Carlo.

# Zobacz również

Możesz zgłębić temat generowania liczb losowych poprzez te źródła:

- Dokumentacja Pythona na temat modułu "random": https://docs.python.org/3/library/random.html
- Strona Wikipedia na temat generatorów liczb losowych: https://pl.wikipedia.org/wiki/Generator_liczb_losowych
- Poradnik "Jak wygenerować losowe liczby w Pythonie": https://www.w3schools.com/python/ref_random.asp