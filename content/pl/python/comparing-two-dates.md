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

## Co i dlaczego?

Porównywanie dat może wydawać się prostym zadaniem, ale jest nieodzowne w programowaniu. Polega ono na porównaniu dwóch dat, aby określić, która jest wcześniejsza lub późniejsza. Programiści wykonują to, aby przetwarzać i porządkować dane w aplikacjach lub aby sprawdzić, czy dana data jest ważna lub aktualna.

## Jak to zrobić:

```Python
from datetime import date

today = date.today()
date1 = date(1997, 9, 12)
date2 = date(2020, 1, 1)

# Porównywanie dat za pomocą operatora ">"
print(date1 > date2) #Wyświetli False
print(date2 > today) #Wyświetli True

# Porównywanie dat za pomocą funkcji "compare"
print(date1.compare(date2)) #Wyświetli -1
print(date2.compare(today)) #Wyświetli 1
```

W powyższym przykładzie używamy modułu `datetime` do utworzenia daty i porównujemy ją z dzisiejszą datą. Możemy wykorzystać operator `>` lub funkcję `compare`, która zwraca wartość -1, 0 lub 1 w zależności od tego, czy pierwsza data jest wcześniejsza, równa lub późniejsza od drugiej.

## Wnikliwe spojrzenie:

Porównywanie dat jest niezbędne nie tylko w programowaniu, ale także w codziennym życiu. W przeszłości programiści musieli pisać skomplikowane funkcje, aby porównywać daty, ale teraz dzięki modułowi `datetime` jest to prostsze. Inną alternatywą jest użycie modułu `dateutil` lub biblioteki `Arrow`. Przy porównywaniu dat należy pamiętać o różnych formatach, na przykład daty zapisanej jako `dd/mm/yyyy` mogą być różne dla różnych języków.

## Zobacz także:

- Dokumentacja modułu `datetime`: https://docs.python.org/3/library/datetime.html
- Porównywanie dat w Pythonie: https://www.geeksforgeeks.org/python-comparing-dates/
- Porównywanie dat w języku polskim: http://python.edu.pl/kursy/python/zadania_14.html