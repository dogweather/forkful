---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:44:57.267112-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Wycinanie podłańcuchów to proces wyodrębniania fragmentów tekstu z większego ciągu znaków. Programiści robią to, aby manipulować i analizować dane tekstowe, weryfikować wprowadzone informacje oraz ułatwić przetwarzanie komunikatów.

## How to:
Do wydobycia podłańcuchów w Arduino służy metoda `substring()`. Poniżej znajdziesz przykłady użycia:

```Arduino
String tekst = "Witaj, Świat!";
String powitanie = tekst.substring(0, 6); // Wydobycie "Witaj,"
String reszta = tekst.substring(7); // Wydobycie "Świat!"

void setup() {
  Serial.begin(9600);
  Serial.println(powitanie); // Wyświetli "Witaj,"
  Serial.println(reszta); // Wyświetli "Świat!"
}

void loop() {
  // Tutaj nic nie umieszczamy.
}
```

## Deep Dive
Wycinanie podłańcuchów istnieje od początków programowania. W Arduino, metoda `substring()` jest wbudowana w klasę `String`, ułatwiając operacje na tekstach. Alternatywą może być użycie operacji na tablicach znaków (C-style strings), które zazwyczaj są szybsze ale bardziej skomplikowane w implementacji. Implementacja `substring()` w Arduino tworzy nowy obiekt `String`, który może wpływać na wydajność i zużycie pamięci, zwłaszcza w większych aplikacjach.

## See Also
Informacje na temat klas i metod w Arduino znajdziesz w oficjalnej dokumentacji online:
- Arduino `String` class reference: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Dodatkowe porady i triki dotyczące pracy z tekstami: [https://www.arduino.cc/en/Tutorial/BuiltInExamples](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
