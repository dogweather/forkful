---
title:    "Python: Porównanie dwóch dat"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się jedną z najważniejszych umiejętności. Jednym z aspektów tego obszaru jest porównywanie dat, które jest nieodłącznym elementem wielu aplikacji. W tym artykule dowiesz się dlaczego takie porównywanie jest ważne i jak to zrobić w języku Python.

## Jak to zrobić

Porównywanie dat w języku Python jest bardzo proste i wymaga użycia specjalnego modułu o nazwie "datetime". Najpierw należy zaimportować ten moduł poleceniem:

```Python
import datetime
```

Następnie, aby utworzyć datę, musimy użyć klasy "date" z tego modułu. Poniżej znajduje się przykład kodu, który tworzy datę 1 stycznia 2021 roku:

```Python
data1 = datetime.date(2021, 1, 1)
```

Aby porównać dwie daty, użyjemy operatorów porównania, takich jak ">" (większe niż) lub "<" (mniejsze niż). Poniższy kod porównuje datę "data1" z aktualną datą:

```Python
if data1 < datetime.date.today():
    print("Data jest z przeszłości.")
else:
    print("Data jest z przyszłości.")
```

Wynik tego kodu będzie zależny od aktualnej daty. Jeśli jest to data wcześniejsza niż 1 stycznia 2021 roku, to pojawi się komunikat "Data jest z przeszłości". W przeciwnym razie, pojawi się komunikat "Data jest z przyszłości".

Możemy również dodawać lub odejmować dni, miesiące i lata do naszej daty, używając metody "replace" oraz klasy "timedelta". Przykładowo:

```Python
data2 = data1.replace(year=2022) #zmiana roku daty "data1"
data3 = data2 + datetime.timedelta(days=30) #dodanie 30 dni do daty "data2"
```

W tym przykładzie, data "data3" będzie równa 30 dni po dacie "data2".

## Deep Dive

Porównywanie dat może być bardziej skomplikowane, jeśli uwzględnimy również informacje o godzinie, minucie i sekundzie. W takim przypadku, należy użyć klasy "datetime" zamiast "date". Poniżej znajduje się przykładowy kod porównujący dwie daty wraz z godzinami:

```Python
czas1 = datetime.datetime(2021, 1, 1, 12, 30, 0) #12:30:00 1 stycznia 2021
czas2 = datetime.datetime(2020, 12, 31, 12, 0, 0) #12:00:00 31 grudnia 2020
if czas1 > czas2:
    print("Czas pierwszy jest późniejszy.")
elif czas1 < czas2:
    print("Czas drugi jest późniejszy.")
else:
    print("Oba czasy są równe.")
```

Wynik tego kodu będzie zależny od porównywanego czasu. W tym przypadku, wypisze się, że "Czas pierwszy jest późniejszy", ponieważ jest to 12:30 1 stycznia 2021 w porównaniu do 12:00 31 grudnia 2020.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat porównywania dat w języku Python, warto zapoznać się z oficjalną dokumentacją modułu "datetime", a także z innymi artykułami na ten temat:

- https://docs.python.org/3/library/datetime.html
- https://www.w3schools.com/python/python_datetime.asp
- https://realpython.com/python-datetime/
- https://www.programiz.com/python-programming/datetime

Dzięki temu będziesz miał pełniejszy obraz na ten temat