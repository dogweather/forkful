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

## Dlaczego

Generowanie losowych liczb jest ważną częścią wielu programów komputerowych i algorytmów. Pozwala nam na symulowanie losowych zdarzeń i tworzenie testowych danych do analizy naszego kodu.

## Jak to zrobić

```Python
import random

print(random.random()) #wyświetli losową liczbę z zakresu 0.0-1.0
print(random.randint(1, 10)) #wyświetli losową liczbę całkowitą z zakresu od 1 do 10
print(random.choice(["jabłko", "banan", "pomarańcza"])) #wyświetli losowy element z listy

```

Przede wszystkim musimy zaimportować moduł random, który jest wbudowany w język Python. Następnie możemy wykorzystać funkcje tego modułu, takie jak `random()`, `randint()` lub `choice()`, aby generować odpowiednie losowe liczby lub elementy z listy. Możemy również ustalić zakres, w którym mają być generowane liczby lub przekazać listę, z której ma być wybrany losowy element.

## Deep Dive

Moduł random wykorzystuje algorytmy do generowania pseudolosowych liczb, które nie są całkowicie losowe. Jednak dla większości zastosowań, takich jak testowanie kodu lub symulowanie losowych zdarzeń, ta metoda jest wystarczająco dokładna. W przypadku potrzeby bardziej zaawansowanych generacji losowych danych, istnieją również inne moduły takie jak `secrets` lub `numpy`, które oferują bardziej skomplikowane algorytmy losowania liczb.

## Zobacz także

- [Dokumentacja modułu random w języku Python](https://docs.python.org/3/library/random.html)
- [Poradnik wideo o generowaniu losowych liczb w języku Python](https://www.youtube.com/watch?v=KzqSDvzOFNA)
- [Artykuł na blogu o wykorzystaniu modułu random w grach komputerowych](https://blog.eduonix.com/software-development/how-to-use-random-module-in-python/)