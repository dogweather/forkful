---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Zamiana i szukanie tekstu polega na lokalizowaniu i podmienianiu wartości tekstowych. Programiści robią to, aby zmodyfikować dane wejściowe na podstawie określonych kryteriów.

## Jak to zrobić
Załóżmy, że chcemy zamienić słowo "Arduino" na "Mikrokontroler" w napisie.
```Arduino
String mojTekst = "Lubie programowanie na Arduino";
mojTekst.replace("Arduino", "Mikrokontroler");
Serial.println(mojTekst);  // Wydrukuje: "Lubie programowanie na Mikrokontroler"
```
Korzystając z funkcji `replace` możemy łatwo zamieniać fragmenty tekstu.

## Zaawansowane informacje
Historia wyszukiwania i zamiany tekstu sięga początków programowania komputerowego. Istnieją różne metody pozwalające na realizację takiego zadania, takie jak wyrażenia regularne (RegEx), które pozwalają na bardziej złożone manipulacje tekstem. W języku Arduino, najprostszą metodą jest użycie wbudowanej funkcji `replace`.

## Zobacz też
[Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/) - Oficjalna dokumentacja funkcji replace w Arduino.
[RegExR](https://regexr.com/) - Narzędzie do nauki i testowania wyrażeń regularnych, które również mogą być użyte do szukania i zamiany tekstu.