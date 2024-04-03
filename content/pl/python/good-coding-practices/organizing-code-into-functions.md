---
date: 2024-01-26 01:11:37.918310-07:00
description: "Organizacja kodu w funkcje polega na podziale kodu na wielokrotnie u\u017C\
  ywalne bloki o okre\u015Blonych celach. Robimy to, aby kod by\u0142 bardziej przejrzysty,\u2026"
lastmod: '2024-03-13T22:44:34.955108-06:00'
model: gpt-4-1106-preview
summary: "Organizacja kodu w funkcje polega na podziale kodu na wielokrotnie u\u017C\
  ywalne bloki o okre\u015Blonych celach."
title: Organizacja kodu w funkcje
weight: 18
---

## Jak to zrobić:
Załóżmy, że piszesz skrypt do obliczania kwadratu i sześcianu liczby. Bez funkcji to nieład powtarzania:

```Python
num = 4
kwadrat = num * num
szescian = num * num * num
print(f"Kwadrat: {kwadrat}, Sześcian: {szescian}")

num = 5
kwadrat = num * num
szescian = num * num * num
print(f"Kwadrat: {kwadrat}, Sześcian: {szescian}")
```
Wynik:
```
Kwadrat: 16, Sześcian: 64
Kwadrat: 25, Sześcian: 125
```

Z funkcjami jest schludniej:

```Python
def kwadrat(n):
    return n * n

def szescian(n):
    return n ** 3

num = 4
print(f"Kwadrat: {kwadrat(num)}, Sześcian: {szescian(num)}")

num = 5
print(f"Kwadrat: {kwadrat(num)}, Sześcian: {szescian(num)}")
```
Wynik:
```
Kwadrat: 16, Sześcian: 64
Kwadrat: 25, Sześcian: 125
```

## Szczegółowy przegląd
Kiedyś, gdy programy były proste, można było obejść się bez funkcji, zapisując po prostu listę instrukcji. Ale w miarę komplikowania się oprogramowania, programiści uświadomili sobie, że wielokrotnie piszą ten sam kod. Witaj, funkcje — wielokrotnie używalne bloki kodu, które wykonują jedno działanie.

Alternatywy dla funkcji obejmują klasy (łączenie funkcji z danymi, na których działają) oraz kod wbudowany (inteligencja tam, gdzie jej potrzebujesz, ale ryzykowna dla skomplikowanych zadań). Jeśli chodzi o implementację, chodzi nie tylko o tworzenie funkcji, ale także o to, aby robiły one jedną rzecz dobrze — pomyśl o zasadzie pojedynczej odpowiedzialności. Funkcje powinny być także idealnie bezstanowe, co oznacza brak niespodzianek z danymi przychodzącymi lub wychodzącymi.

## Zobacz również
- Oficjalne samouczki Pythona na temat funkcji: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- "Czysty kod" autorstwa Roberta C. Martina, zawierający zasady pisania przejrzystych funkcji.
- "Refaktoryzacja: Ulepszanie projektu istniejącego kodu" autorstwa Martina Fowlera, które zawiera przykłady organizowania kodu.
