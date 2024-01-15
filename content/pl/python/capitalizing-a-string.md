---
title:                "Zmiana tekstu na wielkie litery"
html_title:           "Python: Zmiana tekstu na wielkie litery"
simple_title:         "Zmiana tekstu na wielkie litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Może się wydawać, że to prosty i oczywisty krok, ale kapitalizacja ciągów znaków jest bardzo przydatna w programowaniu. Dzięki temu można uzyskać jednolity i czytelny wygląd tekstu wyświetlanego użytkownikowi.

## Jak to zrobić

Kapitalizacja tekstu w Pythonie jest bardzo łatwa i wymaga użycia wbudowanej funkcji `capitalize()`. Przykład użycia wygląda następująco:
```Python
string = "witaj w świecie Pythona"
print(string.capitalize())
```
Wyjściem z powyższego kodu będzie `Witaj w świecie Pythona`. Widzimy tutaj, że funkcja `capitalize()` zmienia tylko pierwszą literę na wielką, pozostawiając resztę tekstu bez zmian. Jeśli chcemy, aby cały tekst był zapisany wielkimi literami, możemy użyć funkcji `upper()`:
```Python
string = "witaj w świecie Pythona"
print(string.upper())
```
Tym razem wynikiem będzie `WITAJ W ŚWIECIE PYTHONA`. Natomiast, jeśli chcemy mieć cały tekst zapisany małymi literami, możemy użyć funkcji `lower()`:
```Python
string = "Witaj w Świecie Pythona"
print(string.lower())
```
Wynikiem będzie `witaj w świecie pythona`.

## Głębszy wgląd

Warto wiedzieć, że funkcje `capitalize()`, `upper()` i `lower()` nie zmieniają oryginalnego tekstu, a jedynie zwracają nowy, zmodyfikowany ciąg znaków. Dzięki temu możemy wykorzystać te funkcje do porównywania tekstów, bez obawy o zmodyfikowanie oryginalnego ciągu.

Inną przydatną funkcją jest `title()`, która zwraca tekst z każdej wyraz pierwszą literą zapisaną wielką. Na przykład:
```Python
string = "witaj w świecie Pythona"
print(string.title())
```
Wynikiem będzie `Witaj W Świecie Pythona`. Warto zauważyć, że w tym przypadku każda wyraz zaczyna się od dużej litery, a nie tylko pierwszy.

Interesującym użyciem funkcji `capitalize()` może być również zamiana wyrazów w odwrotnym porządku. Możemy to osiągnąć korzystając z funkcji `split()` i `join()`:
```Python
string = "Python jest wspaniały"
print(" ".join(word.capitalize() for word in string.split()))
```
Wynikiem będzie `Wspaniały Jest Python`. Dzięki temu możemy tworzyć różne kombinacje i modyfikować tekst w zależności od potrzeb.

## Zobacz także

- [Dokumentacja Pythona dotycząca funkcji `capitalize()`](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [10 przydatnych funkcji do pracy z tekstem w Pythonie](https://realpython.com/python-string-split-concatenate-join/)
- [Porównywanie i modyfikowanie tekstów w Pythonie](https://www.digitalocean.com/community/tutorials/how-to-compare-strings-in-python-3)