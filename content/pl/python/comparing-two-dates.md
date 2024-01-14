---
title:                "Python: Porównywanie dwóch dat"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest częstym wyzwaniem dla programistów. Warto nauczyć się tej umiejętności, ponieważ pozwala ona na efektywne zarządzanie datami w kodzie i uniknięcie błędów związanych z czasem. W tym wpisie dowiesz się, dlaczego porównywanie dat jest ważne i jak to zrobić w języku Python.

## Jak to zrobić

Aby porównać dwie daty w Pythonie, możesz użyć modułu `datetime` oraz jego funkcji `date`. Najpierw musisz utworzyć obiekt `date` dla każdej daty, a następnie użyć operatorów porównania, takich jak `<`, `>` lub `==`, aby je porównać.

Przykładowy kod wyglądałby tak:

```Python
from datetime import date

# tworzenie dat
data_1 = date(2021, 10, 5)
data_2 = date(2021, 10, 10)

# porównywanie dat
if data_1 < data_2:
    print("Data 1 jest wcześniejsza niż data 2.")
elif data_1 > data_2:
    print("Data 2 jest późniejsza niż data 1.")
else:
    print("Obie daty są identyczne.")
```

Output:
```
Data 1 jest wcześniejsza niż data 2.
```

## Deep Dive

Podczas porównywania dat warto wiedzieć, że `date` w Pythonie nie uwzględnia czasu – tylko datę. Oznacza to, że daty z taką samą datą, ale różnym czasem, będą uznane za identyczne.

Możesz również użyć funkcji `max()` i `min()` dla listy dat, aby znaleźć najwcześniejszą lub najpóźniejszą z nich.

Innym przydatnym narzędziem jest moduł `time`, który oferuje funkcję `strptime()` do parsowania daty ze stringu oraz funkcję `strftime()` do formatowania daty do stringu.

Przykładowy kod mógłby wyglądać tak:

```Python
from datetime import date
from time import strptime, strftime

# parsowanie daty ze stringu
data = strptime("2021-10-05", "%Y-%m-%d")

# formatowanie daty do stringu
nowa_data = strftime("%d-%m-%Y", data)
print(nowa_data) # output: 05-10-2021

# znajdowanie najwcześniejszej i najpóźniejszej daty
lista_dat = [date(2021, 10, 5), date(2021, 10, 10), date(2021, 10, 15)]
najwcześniejsza_data = min(lista_dat)
najpóźniejsza_data = max(lista_dat)

print(najwcześniejsza_data) # output: 2021-10-05
print(najpóźniejsza_data) # output: 2021-10-15
```

## Zobacz także

- [Dokumentacja modułu datetime w języku Python](https://docs.python.org/3/library/datetime.html)
- [Porównywanie dat w języku Python](https://realpython.com/python-datetime/)
- [Przykłady użycia modułu time w języku Python](https://www.programiz.com/python-programming/time)