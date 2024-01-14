---
title:    "Python: Porównanie dwóch dat"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Porównywanie dat jest częstym zadaniem w programowaniu. Może to być przydatne, gdy chcemy wyświetlić najnowsze wydarzenia lub sortować informacje chronologicznie. W tym artykule dowiesz się, jak porównywać daty w języku Python i jakie funkcje można wykorzystać do tego celu.

## Jak
Aby porównać dwie daty w Pythonie, musimy najpierw utworzyć obiekty daty za pomocą funkcji `date()` z modułu `datetime`. Możemy przekazać jej rok, miesiąc i dzień, np. `date(2021, 2, 14)` dla daty 14 lutego 2021 roku. Następnie, aby porównać te daty, możemy użyć operatora porównania `>` lub `<` wraz z utworzonymi obiektami daty.

Przykład:
```Python
from datetime import date

# Utworzenie obiektów daty
data_1 = date(2021, 2, 14)
data_2 = date(2021, 3, 1)

# Porównanie dat
if data_1 < data_2:
    print("Data 1 jest wcześniejsza niż data 2")
elif data_2 > data_1:
    print("Data 2 jest późniejsza niż data 1")
else:
    print("Obie daty są takie same")
```
#### Wynik:
```
Data 1 jest wcześniejsza niż data 2
```

Możemy również wykorzystać funkcję `today()` do utworzenia obiektu daty z aktualną datą i porównać go z wcześniej utworzonymi obiektami.

Przykład:
```Python
from datetime import date

# Utworzenie obiektów daty
data_1 = date(2021, 2, 14)
data_2 = date(2021, 3, 1)

# Porównanie dat
dzisiaj = date.today()
if dzisiaj > data_1:
    print("Dzisiaj jest później niż data 1")
elif dzisiaj < data_1:
    print("Dzisiaj jest wcześniej niż data 1")
else:
    print("Data 1 jest dzisiaj")
```
#### Wynik:
```
Dzisiaj jest później niż data 1
```

## Deep Dive
Istnieje wiele funkcji, które mogą być użyteczne podczas porównywania dat w Pythonie. Jedną z nich jest `timedelta()`, która pozwala na przesunięcie daty o określoną liczbę dni lub innych jednostek. Możemy wykorzystać tę funkcję do stworzenia zakresu dat i przeiterować przez nie porównując je z innymi datami.

Kolejną przydatną funkcją jest `strftime()`, która pozwala na formatowanie daty w różnych formatach. Na przykład, `data.strftime("%d/%m/%Y")` zwróci datę w formacie "14/02/2021". Możemy wykorzystać tę funkcję do wyświetlania dat w bardziej czytelny sposób.

## Zobacz także
- Dokumentacja modułu datetime w języku Python: https://docs.python.org/3/library/datetime.html
- Szybki tutorial o pracy z datami w Pythonie: https://www.programiz.com/python-programming/datetime
- Porównywanie dat i utworzenie zakresu dat w Pythonie: https://www.geeksforgeeks.org/python-combining-two-different-datetime-datetime-objects-using-datetime-timedelta/
- Inne przydatne funkcje z modułu datetime: https://www.programiz.com/python-programming/datetime#functions