---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Python: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym krokiem w wielu programach, szczególnie w zakresie przetwarzania danych i tworzenia raportów. Jest to przydatne w celu uporządkowania i wyświetlenia informacji w czytelny sposób dla użytkowników.

## Jak to zrobić

```Python
import datetime

now = datetime.datetime.now()
print("Dzisiejsza data to: " + str(now))
```

Output: "Dzisiejsza data to: 2021-08-11 16:22:40.526995"

Aby skonwertować datę na ciąg znaków, należy użyć funkcji "str()" wraz z obiektem datetime. W tym przypadku, używając funkcji "now()", otrzymujemy aktualną datę i godzinę jako obiekt datetime, który następnie jest konwertowany na ciąg znaków za pomocą funkcji "str()" i wyświetlany na ekranie.

Możemy również określić format daty, wykorzystując metodę "strftime()":

```Python
print("Dzisiaj jest " + now.strftime("%d/%m/%Y"))
```

Output: "Dzisiaj jest 11/08/2021"

W wyżej wymienionym przykładzie, ustaliliśmy format daty jako dzień/miesiąc/rok, używając odpowiednich symboli (%d - dzień, %m - miesiąc, %Y - rok) w metodzie "strftime()".

## Deep Dive

Kluczowym elementem konwersji daty na ciąg znaków jest zrozumienie modułu datetime w Pythonie. Moduł ten zawiera wiele przydatnych funkcji i metod do zarządzania datami, wliczając w to konwersję na ciąg znaków.

Należy pamiętać, że dane daty muszą być poprawnie zdefiniowane, aby móc je przetwarzać i wypisywać w oczekiwanym formacie. W Pythonie, obiekt datetime jest uważany za niezmienny, co oznacza, że po utworzeniu, nie może być modyfikowany. Możemy jednak używać różnych metod, takich jak "replace()", aby zmieniać poszczególne składowe daty, na przykład rok czy godzinę.

## Zobacz też

- Dokumentacja modułu datetime w Pythonie: https://docs.python.org/3/library/datetime.html
- Przewodnik po konwersji daty na ciąg znaków w Pythonie: https://realpython.com/python-datetime/
- Przykłady zastosowań konwersji daty na ciąg znaków: https://www.w3schools.com/python/python_datetime.asp