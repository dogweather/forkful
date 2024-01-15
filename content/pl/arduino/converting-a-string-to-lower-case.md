---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Arduino: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja łańcucha znaków na małe litery jest często niezbędna w programowaniu, szczególnie w przypadku pracy z tekstem. Pozwala to na ujednolicenie danych i ułatwia dalsze przetwarzanie. W ten sposób można również uniknąć problemów z błędnym rozpoznawaniem słów czy znaków.

## Jak to zrobić

Konwersja łańcucha znaków na małe litery jest możliwa przy użyciu funkcji `toLowerCase()`, która jest dostępna w języku Arduino. Poniżej przedstawiono przykładowy kod, który wprowadzi cię w temat.

```Arduino
String slowo = "KOT";
String male_litery = slowo.toLowerCase();

Serial.println(male_litery); // wyświetli "kot"
```

Funkcja `toLowerCase()` zamienia wszystkie litery w łańcuchu na ich odpowiedniki w małych literach. Jeśli potrzebujesz tylko pierwszą literę zmienić na małą, możesz skorzystać z funkcji `toLowercase[0]`.

## Dogłębna analiza

Zwróć uwagę, że funkcja `toLowerCase()` zwraca kopię otrzymanego łańcucha, a nie modyfikuje go bezpośrednio. Jeśli chcesz zmienić oryginalny łańcuch, musisz przypisać wartość zwróconą przez funkcję do zmiennej, tak jak w przykładzie powyżej.

Ponadto, funkcja `toLowerCase()` nie działa tylko na pojedynczych znakach, ale również na całych łańcuchach. Możesz więc przekazać do niej zmienną typu `String` lub wprowadzić wartość bezpośrednio.

Innym sposobem na konwersję łańcucha znaków na małe litery jest użycie pętli `for` i funkcji `isAlpha()`, która sprawdza, czy dany znak jest literą. Należy jednak pamiętać, że ta metoda wymaga więcej kodu i może być mniej wydajna.

## Zobacz również

- [Dokumentacja funkcji `toLowerCase()` w języku Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/tostring/)
- [Tutorial o konwersji łańcuchów znaków na małe litery w języku C](https://www.programiz.com/c-programming/library-function/string/tolower)