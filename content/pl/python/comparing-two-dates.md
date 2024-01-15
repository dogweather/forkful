---
title:                "Porównywanie dwóch dat"
html_title:           "Python: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest nieodłączną częścią wielu zastosowań programowania w języku Python. Jest to szczególnie przydatne w przypadkach, gdy chcemy sprawdzić, czy określona data jest wcześniejsza lub późniejsza od innej daty. W tym artykule przejdziemy przez podstawowe techniki porównywania dat w Pythonie.

## Jak to zrobić

Porównywanie dat w Pythonie jest możliwe dzięki wbudowanej funkcji ```date``` oraz modułowi ```datetime```, który pozwala na wygodne manipulowanie czasem i datami.

Aby porównać dwie daty, musimy najpierw utworzyć zmienne przechowujące konkretne daty. Możemy to zrobić na kilka sposobów, np.:

```Python
date_1 = datetime.date(2020, 7, 15)
date_2 = datetime.date(2019, 11, 30)
```

Następnie wykorzystujemy operator porównania (np. ```>``` lub ```<```) do porównania tych dwóch zmiennych, np.:

```Python
date_1 > date_2 # zwraca "True", ponieważ 15 lipca 2020 jest później niż 30 listopada 2019
```

Możemy również wykorzystać funkcję ```timedelta``` z modułu ```datetime``` do dodawania lub odejmowania określonej ilości dni, tygodni czy miesięcy do daty. Na przykład:

```Python
date_3 = datetime.date(2020, 1, 1)
date_4 = date_3 + datetime.timedelta(30) # dodajemy 30 dni do daty 1 stycznia 2020
```

## Głębszy zanurzenie

W Pythonie istnieją również bardziej zaawansowane metody porównywania dat, takie jak wykorzystanie klasy ```dateutil.parser``` do konwersji napisów na daty, czy użycie metody ```strptime()``` do parsowania dat zgodnie z określonym formatem.

Warto również wiedzieć, że daty w Pythonie mogą być porównywane nie tylko za pomocą operatorów porównania, ale także z użyciem metod ```equal()```, ```isocalendar()``` czy ```toordinal()```, które pozwalają na bardziej precyzyjne porównanie.

## Zobacz także
[Oficjalna dokumentacja Pythona dotycząca modułu ```datetime```](https://docs.python.org/3/library/datetime.html)

[Poradnik na temat porównywania dat w Pythonie](https://realpython.com/python-datetime/)

[Przykłady kodu z wykorzystaniem modułu ```datetime```](https://www.programiz.com/python-programming/datetime)

[Porównywanie dat w innych językach programowania: różnice i podobieństwa](https://www.freecodecamp.org/news/the-basic-principles-of-date-and-time-comparison-in-programming/)