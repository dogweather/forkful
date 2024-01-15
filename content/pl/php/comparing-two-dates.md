---
title:                "Porównywanie dwóch dat"
html_title:           "PHP: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest niezwykle przydatną umiejętnością w programowaniu, ponieważ pozwala nam na łatwe sprawdzanie, czy dwa wydarzenia miały miejsce w przeszłości, teraz lub w przyszłości. Jest to również często wykorzystywane przy filtrowaniu i sortowaniu danych w bazach danych.

## Jak to zrobić

```PHP
// Utwórz dwa obiekty daty
$date1 = new DateTime('2020-01-01');
$date2 = new DateTime('2021-01-01');

// Sprawdź, czy $date1 jest przed $date2
if ($date1 < $date2) {
    echo "Data 1 jest wcześniejsza niż data 2.";
}

// Sprawdź, czy $date2 jest po $date1
if ($date2 > $date1) {
    echo "Data 2 jest późniejsza niż data 1.";
}
```

Przykładowy wynik:
```
Data 1 jest wcześniejsza niż data 2.
Data 2 jest późniejsza niż data 1.
```

## Głębsze zagadnienia

Podczas porównywania dat istnieje kilka ważnych rzeczy do rozważenia. Po pierwsze, należy pamiętać o strefie czasowej. Jeśli np. porównujemy daty pochodzące z różnych krajów, powinniśmy najpierw skonwertować je do wspólnej strefy czasowej, aby uzyskać dokładne wyniki. Kolejną ważną kwestią jest uwzględnienie lat przestępnych. Aby uniknąć błędów, należy upewnić się, że obie daty znajdują się w poprawnym formacie, a także dokładnie określić, czy chodzi o porównywanie dat, a nie daty i czasu.

## Zobacz również

- [Dokumentacja PHP - klasa DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Porównywanie dat w PHP](https://www.php.net/manual/en/datetime.diff.php)
- [Porównywanie i sortowanie dat w bazie danych MySQL](https://www.w3schools.com/sql/sql_dates.asp)