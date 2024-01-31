---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zamiana łańcucha znaków na wielkie litery oznacza, że wszystkie litery w tekście stają się dużymi literami. Programiści używają tej techniki, by ujednolicić dane, na przykład żeby nazwy użytkowników były spójne niezależnie od sposobu wprowadzenia.

## How to: (Jak to zrobić:)
```Arduino
void setup() {
  Serial.begin(9600); 
  String myString = "Witaj, Arduino!";
  Serial.println(capitalize(myString)); 
}

void loop() {
  // Tę funkcję zostawiamy pustą, w naszym przykładzie nie będzie używana.
}

String capitalize(String input) {
  for (int i = 0; i < input.length(); i++) {
    input[i] = toupper(input[i]);
  }
  return input;
}
```

Output:
```
WITAJ, ARDUINO!
```

## Deep Dive (Dogłębna analiza)
Zamiana tekstu na same wielkie litery nie jest nowym pomysłem – używa się jej od czasów maszyn do pisania, aby podkreślić ważne fragmenty tekstu. Na Arduino i innych platformach mikrokontrolerów często brakuje wbudowanych funkcji do manipulowania tekstami, co wymusza na programistach pisanie własnych funkcji, takich jak `capitalize`. Alternatywne podejścia mogłyby polegać na użyciu funkcji zewnętrznych bibliotek, lecz to zwiększa rozmiar i złożoność kodu. Przy implementacji pamiętaj, że `String` w Arduino jest obiektem, który zarządza dynamiczną pamięcią, a nieprawidłowe jej używanie może prowadzić do fragmentacji pamięci.

## See Also (Zobacz również)
- Dokumentacja Arduino `String` Class: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Funkcje manipulujące ciągami znaków w C++: http://www.cplusplus.com/reference/cstring/
- Wprowadzenie do zarządzania pamięcią w Arduino: https://learn.adafruit.com/memories-of-an-arduino/more-memories
