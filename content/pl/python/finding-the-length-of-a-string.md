---
title:    "Python: Znajdowanie długości ciągu znaków"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w czasie programowania zdarza się, że musimy pracować z różnymi tekstami. W niektórych przypadkach może być istotne poznanie długości tych tekstów. Dlatego też warto poznać sposoby na znalezienie długości łańcucha znaków w języku Python.

## Jak to zrobić

Istnieje kilka sposobów na znalezienie długości łańcucha znaków w Pythonie.

Możemy wykorzystać funkcję `len()`, która zwraca liczbę znaków w łańcuchu. Przykładowy kod wyglądałby następująco:

```Python
string = "To jest przykładowy tekst"
length = len(string)
print("Długość tekstu to:", length)
```

Output:
```
Długość tekstu to: 24
```

Inną opcją jest użycie metody `count()`, która zlicza określony znak lub sekwencję znaków w łańcuchu. Przykładowy kod wyglądałby tak:

```Python
string = "Przykładowy tekst"
count = string.count("k")
print("Liczba wystąpień k w tekście:", count)
```

Output:
```
Liczba wystąpień k w tekście: 2
```

Możemy również wykorzystać pętlę `for` w połączeniu z metodą `range()` i sprawdzić każdy znak w łańcuchu:

```Python
string = "Ala ma kota"
count = 0
for i in range(len(string)):
    if string[i] == "a":
        count += 1
print("Liczba wystąpień a w tekście:", count)
```

Output:
```
Liczba wystąpień a w tekście: 4
```

## Deep Dive

W języku Python łańcuchy znaków są traktowane jako sekwencje, co oznacza, że każdy znak w łańcuchu ma przypisaną swoją pozycję, czyli indeks. Pierwszy znak ma indeks 0, drugi 1, itd.

Dzięki temu możemy wykorzystać wiedzę na temat sekwencji i korzystać z funkcji i metod dostępnych w języku Python.

## Zobacz również

- [Dokumentacja Python - struktury danych](https://docs.python.org/pl/3/tutorial/datastructures.html)
- [Kurs Python na Codecademy](https://www.codecademy.com/learn/learn-python)