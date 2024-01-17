---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Arduino: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwertowanie ciągu znaków na małe litery jest procesem polegającym na zmianie wszystkich liter w ciągu na ich małe odpowiedniki. Jest to często wykonywane przez programistów w celu ułatwienia porównywania i analizowania tekstów, gdyż litery wielkie i małe są traktowane jako różne znaki.

## Jak to zrobić:
Przykładowy kod do konwersji ciągu znaków na małe litery w Arduino wygląda następująco:
```
String tekst = "HELLO WORLD";
tekst.toLowerCase();
Serial.println(tekst); // wypisze "hello world"
```

Można też użyć funkcji write() do wypisania ciągu w postaci małych liter:
```
String tekst = "HELLO WORLD";
for(int i = 0; i < tekst.length(); i++){
  Serial.write(tolower(tekst[i])); // wypisze "hello world"
}
```

## Dogłębna analiza:
Konwersja ciągu znaków na małe litery ma swoje korzenie w informatyce, kiedy to programiści musieli ręcznie zmieniać wielkość liter w tekście w celu poprawnego porównania lub sortowania. Obecnie istnieją również inne metody konwersji, takie jak użycie liblconv lub metod wbudowanych w inne języki programowania.

## Zobacz też:
- Dokumentacja Arduino String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Alternatywne metody konwersji: https://www.oreilly.com/library/view/c-cookbook/0596003390/ch08s16.html