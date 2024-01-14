---
title:                "Gleam: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest niezwykle przydatną umiejętnością, zarówno w programowaniu, jak i w życiu codziennym. Może być wykorzystywane do tworzenia gier, symulacji, testowania algorytmów i wielu innych zastosowań. W tym wpisie dowiesz się, jak w prosty sposób generować liczby losowe w języku programowania Gleam.

## Jak to zrobić

Aby wygenerować losową liczbę w Gleam, musimy użyć wbudowanej biblioteki `random` za pomocą polecenia `import random`.

```Gleam
import random

random.int(0, 100)
// Wynik: 42
```

W powyższym przykładzie korzystamy z funkcji `int` z biblioteki `random`, dzięki czemu wygenerowana zostanie liczba całkowita w podanym zakresie (od 0 do 100). Możemy oczywiście dostosować ten zakres do swoich potrzeb.

```Gleam
random.float(0.0, 10.0)
// Wynik: 6.834
```

W tym przykładzie używamy funkcji `float` do wygenerowania liczby zmiennoprzecinkowej w zakresie od 0.0 do 10.0.

## Dogłębna analiza

Podczas korzystania z funkcji `random` w Gleam, warto zwrócić uwagę na kilka ważnych detali. Po pierwsze, warto mieć świadomość, że funkcje te generują jedynie pseudolosowe liczby, co oznacza, że są one obliczane matematycznie i nie są w pełni losowe. Jednak w większości przypadków taka precyzja jest wystarczająca.

Dodatkowo, funkcje `random` nie są w stanie wygenerować liczb losowych w sposób ciągły, tzn. za każdym razem, gdy uruchamiamy funkcję w tej samej sytuacji, otrzymamy tę samą liczbę. Aby uniknąć tego problemu, można wykorzystać generator liczb losowych `seeded`, który pozwala na bardzo precyzyjną kontrolę nad wygenerowanymi liczbami.

## Zobacz także

- Dokumentacja biblioteki `random`: https://gleam.run/modules/random
- Poradnik o wykorzystaniu funkcji `random` w języku Python: https://realpython.com/python-random/
- Ciekawy artykuł o zastosowaniu liczb losowych w programowaniu: https://www.smashingmagazine.com/2019/05/randomness-in-design/