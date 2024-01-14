---
title:                "Gleam: Konwertowanie daty na ciąg znaków"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków może wydawać się męczącym zadaniem, ale w rzeczywistości jest to niezbędna umiejętność w programowaniu. Dzięki niej możemy wyświetlać daty w czytelnej formie oraz manipulować nimi w naszych programach. W tym artykule pokażemy, jak w prosty sposób konwertować daty na ciągi znaków w języku programowania Gleam.

## Jak to zrobić

Aby skonwertować datę na ciąg znaków w Gleam, musimy użyć funkcji `string_format` z modułu `gleam/time` oraz podać odpowiedni format daty. Przykładowy kod wyglądałby następująco:

```Gleam
import gleam/time

let date = time.now()

let formatted_date = time.string_format(
  "%d.%m.%Y",
  date
)

// Output: 25.10.2021
```

W powyższym kodzie najpierw importujemy moduł `gleam/time`, a następnie używamy funkcji `now()` aby pobrać bieżącą datę. Następnie, jako pierwszy argument funkcji `string_format`, podajemy format daty. W tym przypadku użyliśmy `%d` aby wyświetlić dzień, `%m` aby wyświetlić miesiąc, a `%Y` aby wyświetlić rok. W drugim argumencie podajemy samą datę, którą chcemy skonwertować. Otrzymujemy w rezultacie 25.10.2021, ponieważ bieżącą datą w momencie pisania tego artykułu jest 25 października 2021.

W przypadku gdy chcemy dodać do daty również informacje o godzinie, możemy skorzystać z formatu `%H:%M:%S`, który wyświetli godzinę, minuty i sekundy. Kod wyglądałby wtedy tak:

```Gleam
let formatted_date_with_time = time.string_format(
  "%d.%m.%Y %H:%M:%S",
  date
)

// Output: 25.10.2021 10:30:00
```

## Deep Dive

Jeśli chcemy dokładniej poznać możliwości formatowania dat w Gleam, warto przyjrzeć się dostępnym formatom. Pełna lista dostępnych formatów wraz z opisami znajduje się w oficjalnej dokumentacji języka Gleam [tutaj](https://gleam.run/documentation/modules/time#date_format).

Warto również zapoznać się z funkcją `parse` z tego samego modułu, która pozwala na przetworzenie ciągu znaków na obiekt daty, co może być przydatne w niektórych sytuacjach.

## Zobacz również

- Oficjalna dokumentacja modułu `time` w języku Gleam (https://gleam.run/documentation/modules/time)
- Przykładowe kody konwertujące daty na ciągi znaków w innych językach programowania (https://www.w3schools.com/jsref/jsref_tostring_date.asp)