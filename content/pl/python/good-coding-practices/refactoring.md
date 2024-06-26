---
date: 2024-01-26 03:37:42.435844-07:00
description: "Jak to zrobi\u0107: Za\u0142\xF3\u017Cmy, \u017Ce masz kawa\u0142ek\
  \ kodu, kt\xF3ry oblicza i drukuje powierzchni\u0119 i obw\xF3d prostok\u0105ta\
  \ na podstawie jego d\u0142ugo\u015Bci i szeroko\u015Bci. Robi to\u2026"
lastmod: '2024-03-13T22:44:34.958318-06:00'
model: gpt-4-0125-preview
summary: "Za\u0142\xF3\u017Cmy, \u017Ce masz kawa\u0142ek kodu, kt\xF3ry oblicza i\
  \ drukuje powierzchni\u0119 i obw\xF3d prostok\u0105ta na podstawie jego d\u0142\
  ugo\u015Bci i szeroko\u015Bci."
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:
Załóżmy, że masz kawałek kodu, który oblicza i drukuje powierzchnię i obwód prostokąta na podstawie jego długości i szerokości. Robi to zadanie, ale jest powtarzalny i trochę nieporządny.

```python
# Oryginalna wersja
length = 4
width = 3

# Obliczanie powierzchni i obwodu
area = length * width
perimeter = 2 * (length + width)

print("Powierzchnia:", area)
print("Obwód:", perimeter)
```

Możemy to zrefaktoryzować, enkapsulując funkcjonalność w funkcje, co czyni kod bardziej zorganizowanym i możliwym do ponownego użycia:

```python
# Zrefaktoryzowana wersja

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# użycie
length = 4
width = 3

print("Powierzchnia:", calculate_area(length, width))
print("Obwód:", calculate_perimeter(length, width))
```

Oba fragmenty kodu wypisują ten sam wynik:
```
Powierzchnia: 12
Obwód: 14
```

Ale zrefaktoryzowana wersja jest czystsza i rozdziela obawy, co ułatwia aktualizację jednego obliczenia bez wpływu na drugie.

## Pogłębienie
Refaktoryzacja ma swoje korzenie we wczesnych dniach inżynierii oprogramowania, gdy programiści zdali sobie sprawę, że kod może—i powinien—być ulepszony, nawet jeśli już "działa". Książka Martina Fowlera "Refaktoryzacja: Ulepszanie projektu istniejącego kodu" przedstawiła wiele podstawowych zasad i technik. Słynnie powiedział: "Każdy głupiec może napisać kod, który komputer zrozumie. Dobrzy programiści piszą kod, który ludzie mogą zrozumieć."

Alternatywy dla refaktoryzacji mogą obejmować pisania kodu od nowa lub dokonywanie drobnych poprawek bez systematycznego ulepszania. Jednakże, refaktoryzacja jest zazwyczaj bardziej opłacalna niż przepisywanie od nowa i mniej ryzykowna niż ad-hoc modyfikacje. Szczegóły implementacji mogą być specyficzne dla każdego paradygmatu programowania; jednak programowanie zorientowane obiektowo szczególnie dobrze nadaje się do refaktoryzacji, szczególnie z technikami takimi jak wyodrębnianie metod (jak nasze funkcje `calculate_area` i `calculate_perimeter`), inlining, przenoszenie funkcji między obiektami oraz zmiana nazw metod lub zmiennych dla większej jasności.

Refaktoryzacja w Pythonie często wykorzystuje narzędzia takie jak `PyCharm`, który ma wbudowane możliwości refaktoryzacji, lub `rope`, bibliotekę Pythona specjalnie zaprojektowaną do refaktoryzacji. Ostrożne korzystanie z kontroli wersji, takiej jak `git`, podczas refaktoryzacji jest bardzo zalecane, aby śledzić zmiany stopniowo.

## Zobacz również
Dla tych, którzy chcą więcej, zanurz się w następujące zasoby:
- Książka Martina Fowlera: [Refaktoryzacja: Ulepszanie projektu istniejącego kodu](http://www.refactoring.com/)
- Refaktoryzacja Pythona z `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- Dokumentacja refaktoryzacji PyCharm: [Jetbrains PyCharm Refactor Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refaktoryzacja i wzorce projektowe](https://refactoring.guru/refactoring)
- Wykłady o czystym kodzie przez Uncle Boba (Robert C. Martin): [Czysty kod - Uncle Bob / Lekcja 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
