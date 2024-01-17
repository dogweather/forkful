---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków to zwykła czynność wykonywana przez programistów, polegająca na obliczeniu liczby znaków w ciągu. Jest to ważna umiejętność, która pomaga w manipulowaniu i porównywaniu różnych ciągów znaków w programach.

## Jak to zrobić:
Arduino ma wbudowaną funkcję `strlen()`, która zwraca długość ciągu znaków. Przykładowo, jeśli chcesz wyświetlić liczbę znaków w zmiennej `string`, użyj:
```
Arduino
Serial.println(strlen(string));
```
Ten kod zwróci liczbę całkowitą reprezentującą długość ciągu znaków.

## Zagłębienie się w temat:
Znajdowanie długości ciągu znaków jest ważnym elementem w programowaniu, ponieważ pozwala na porównywanie i analizowanie różnych ciągów znaków w celu wykonywania odpowiednich operacji. Alternatywą dla funkcji `strlen()` w Arduino jest funkcja `sizeof()`, która zwraca rozmiar zmiennej w bajtach. Jednak `sizeof()` może zwrócić niewłaściwą wartość w przypadku zmiennych o typie danych `string`.

## Zobacz również:
- [Podstawy programowania w Arduino](https://www.arduino.cc/en/Tutorial/Foundations)
- [Pełny podręcznik Arduino](https://www.arduino.cc/reference/en/)
- [Jak zacząć z Arduino](https://www.arduino.cc/en/Guide/HomePage)