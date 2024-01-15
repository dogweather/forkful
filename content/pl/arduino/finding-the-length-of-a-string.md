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

## Dlaczego

Jeśli pracujesz z ciągami znaków w Arduino, być może zadam sobie pytanie: "Jak mogę sprawdzić długość danego ciągu?". Jest to przydatna umiejętność, ponieważ może Ci pomóc w manipulowaniu i porównywaniu różnych ciągów, co może być użyteczne w wielu projektach Arduino.

## Jak to zrobić

Sprawdzenie długości ciągu w Arduino jest proste. Wystarczy użyć funkcji ```length()```, która zwraca liczbę znaków w danym ciągu. Możesz użyć tej funkcji w połączeniu z instrukcją ```Serial.println()``` do wyświetlenia wyniku.

```Arduino
// Przykładowy kod do wyświetlenia długości ciągu z domyślnego tekstu
String tekst = "Przykładowy tekst";
Serial.println(tekst.length()); // Wynik: 18
```

## Głębsza analiza

W Arduino funkcja ```length()``` jest dostępna dla obiektów typu ```String```, dlatego musisz ustawić typ daty na ```String```, aby móc jej użyć. Jeśli chcesz wyświetlić długość ciągu liczbowego, musisz najpierw przekonwertować go na typ ```String```, a następnie wykorzystać funkcję ```length()```. 

Funkcja ta jest również przydatna przy porównywaniu dwóch ciągów, ponieważ możesz użyć jej do sprawdzenia czy mają one taką samą długość. Jeśli chcesz porównać tylko część ciągu, możesz użyć funkcji ```substring()```, która zwraca podciąg o określonej długości.

## Zobacz także

Może zainteresować Cię również temat "Manipulowanie ciągami znaków w Arduino", który zawiera dodatkowe informacje na temat pracy z ciągami w Arduino.

- [Manipulowanie ciągami znaków w Arduino - Dokumentacja Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Funkcja length() w Arduino - Instructables](https://www.instructables.com/Find-Character-Length-of-String-in-Arduino/)
- [Działanie funkcji substring() w Arduino - Arduino Stack Exchange](https://arduino.stackexchange.com/questions/253/how-can-i-get-a-substring-from-a-string)