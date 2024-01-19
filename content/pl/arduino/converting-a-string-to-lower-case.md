---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja łańcucha na małe litery oznacza zmianę wszystkich dużych liter na ich odpowiedniki w niższej kapsie. Programiści robią to często, aby normalizować dane wejściowe, ułatwiając porównywanie ciągów.

## Jak to zrobić:

Podajmy przykład użycia funkcji `toLowerCase()`, która działa z dowolnym ciągiem znaków na Arduino.

```arduino
String mojTekst = "Czesc, Arduino!";
mojTekst.toLowerCase();
Serial.begin(9600);
Serial.println(mojTekst); // wydrukuje: "czesc, arduino!"
```

Wywołanie `toLowerCase()` konwertuje wszystkie litery w stringu na małe litery. 

## W głąb tematu:

Funkcja `tolower ()` pochodzi z języka C, w którym była używana do konwersji pojedynczych znaków, a nie ciągów. 

Alternatywnie, można przekształcić jakiś ciąg na małe litery "ręcznie", przechodząc przez każdy znak ciągu i używając funkcji `tolower()`, z biblioteki ctype.h.

Szczególnie ważne jest zrozumienie, że `toLowerCase()` działa na oryginalnym ciągu (mutuje go), zamiast tworzyć nową kopię z wynikiem.

## Zobacz także:

- Dokumentacja Arduino na temat String toLowerCase(): https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/
- Świetne wprowadzenie do manipulacji ciągamia: http://www.cplusplus.com/reference/cctype/tolower/