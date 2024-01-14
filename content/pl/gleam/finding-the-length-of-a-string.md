---
title:    "Gleam: Znajdowanie długości ciągu znaków"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie dowiesz się, dlaczego jest ważne znalezienie długości łańcucha w języku programowania Gleam. Poznasz również przykładowy kod i wyniki jego wykonania.

## Jak to zrobić

```Gleam let string = "Witaj, świecie!"``` 
```Gleam let length = String.length(string)```
```Gleam IO.print("Długość łańcucha to ", length)```

Wynik:
```
Długość łańcucha to 14
```

Możemy użyć funkcji `String.length()` aby znaleźć długość dowolnego łańcucha w języku Gleam. Jest to przydatne, jeżeli chcemy sprawdzić, czy łańcuch spełnia wymaganą liczbę znaków przed przetwarzaniem go dalej w kodzie.

## Głębsza analiza

Funkcja `String.length()` zwraca liczbę znaków w danym łańcuchu, włączając w to spacje, przecinki i inne znaki. Jeśli chcemy zignorować spacje, możemy użyć funkcji `String.trim()`, aby usunąć je z łańcucha przed obliczeniem długości.

Pamiętaj, że długość łańcucha jest różna dla różnych języków. Na przykład, w języku polskim litera "ą" może zostać zliczona jako dwa znaki, co może wpłynąć na wynik funkcji `String.length()`.

## Zobacz także

- Dokumentacja języka Gleam: https://gleam.run/
- Funkcje dla typu `String`: https://gleam.run/docs/std-lib/string
- Kodeks postępowania dla programistów w języku Gleam: https://gleam.run/contributors-guide/code-of-conduct