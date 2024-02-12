---
title:                "Organizacja kodu w funkcje"
aliases:
- /pl/python/organizing-code-into-functions.md
date:                  2024-01-26T01:11:37.918310-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizacja kodu w funkcje"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Organizacja kodu w funkcje polega na podziale kodu na wielokrotnie używalne bloki o określonych celach. Robimy to, aby kod był bardziej przejrzysty, łatwiejszy do odczytania, debugowania i aktualizacji.

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
