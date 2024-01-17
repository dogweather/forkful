---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Python: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego? 
Konwersja ciągu znaków na małe litery jest jedną z podstawowych operacji wykonywanych przez programistów w języku Python. Polega to na zmniejszeniu wszystkich liter w ciągu znaków do ich odpowiedników w małych literach. Programiści wykonują tę operację, aby ujednolicić dane i ułatwić porównywanie i dopasowywanie ciągów znaków.

## Jak to zrobić: 
Aby skorzystać z funkcji konwersji ciągu znaków na małe litery w języku Python, należy użyć metody ```lower()```. Przykład:

```
ciag_znakow = "Hello World"
print(ciag_znakow.lower())

```

Output: hello world

Możemy również zastosować tę funkcję do zmiennej, która przechowuje dane wprowadzone przez użytkownika:

```
uzytkownik = input("Podaj swoje imię: ")
print("Witaj, " + uzytkownik.lower())

```

Output: Witaj, [imię użytkownika w małych literach]

## Głębszy rozkład: 
Konwersja ciągu znaków na małe litery jest często wykonana w celu porównania dwóch ciągów znaków bez uwzględniania wielkości liter. Jest to szczególnie przydatne przy przetwarzaniu tekstów, na przykład w wyszukiwaniu lub sortowaniu danych. Alternatywą dla użycia funkcji ```lower()``` jest użycie metody ```casefold``` lub modułu ```unicodedata```, które oferują większą elastyczność w traktowaniu różnych języków i systemów znaków. Implementacja funkcji ```lower()``` jest częścią standardowej biblioteki języka Python, więc nie wymaga dodatkowych importów.

## Zobacz także: 
[Atrybuty metod stosowanych do ciągów znaków w języku Python](https://docs.python.org/3/library/stdtypes.html#string-methods) | [Jakieś pytania? Przemarzaj Python!](https://www.python.org/)