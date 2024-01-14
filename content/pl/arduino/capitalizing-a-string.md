---
title:                "Arduino: Zapisywanie dużych liter w ciągu znaków"
simple_title:         "Zapisywanie dużych liter w ciągu znaków"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy manipulować ciągami znaków, czyli sekwencjami liter i znaków. Jednym z zadań, które możemy wykonać na ciągach, jest zamiana pierwszej litery na wielką. To może być przydatne na przykład w pisaniu tytułów lub informacji o produkcie.

## Jak to zrobić

Aby skorzystać z funkcji do zamiany pierwszej litery na wielką w Arduino, musimy dodać bibliotekę <string.h>. Następnie możemy użyć funkcji ```capitalize()```, podając jako argument żądany ciąg znaków.

```Arduino
#include <string.h>

String tekst = "witaj świecie!";
tekst.capitalize();
Serial.println(tekst); // Output: Witaj świecie!
```

## Głębsze zanurzenie

Funkcja ```capitalize()``` w rzeczywistości zmienia tylko pierwszą literę ciągu na wielką. Jeśli chcemy zmienić wszystkie litery w ciągu na wielkie, możemy użyć funkcji ```toUpperCase()```.

```Arduino
tekst.toUpperCase();
Serial.println(tekst); // Output: WITAJ ŚWIECIE!
```

Funkcje ```capitalize()``` i ```toUpperCase()``` są dostępne również dla typu danych ```char```, a nie tylko dla typu ```String```. W przypadku ```char```, funkcje te zwrócą zmodyfikowany ciąg jako wynik.

## Zobacz również

- [Dokumentacja biblioteki <string.h> w Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/)
- [Przykładowy kod z zastosowaniem funkcji capitalize()](https://create.arduino.cc/projecthub/TopTechnologies/using-string-capitalize-function-8d2d78)
- [Inne funkcje dostępne w bibliotece <string.h>](https://www.robotics.org.za/str.htm)