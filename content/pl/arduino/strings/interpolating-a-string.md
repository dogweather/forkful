---
title:                "Interpolacja łańcuchów znaków"
aliases:
- /pl/arduino/interpolating-a-string/
date:                  2024-01-20T17:50:08.130580-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Interpolacja ciągu znaków umożliwia wstawianie zmiennych bezpośrednio w tekst. Używamy jej, by ułatwić tworzenie dynamicznych wiadomości i formatować je czytelnie.

## How to: (Jak to zrobić:)
Arduino nie ma natywnej funkcji interpolacji ciągów, ale możemy osiągnąć podobny efekt używając `sprintf` lub łącząc ciągi przy użyciu operatora `+`.

```Arduino
char buffer[50];
int temperature = 23;

void setup() {
  Serial.begin(9600);
  
  // Użycie sprintf do wstawienia zmiennej do ciągu
  sprintf(buffer, "Temperatura to: %d stopni Celsjusza.", temperature);
  Serial.println(buffer);

  // Alternatywne łączenie ciągów
  String message = "Temperatura to: " + String(temperature) + " stopni Celsjusza.";
  Serial.println(message);
}

void loop() {
  // Nic nie robimy w pętli.
}
```
Wyjście jest takie samo dla obu metod:
```
Temperatura to: 23 stopni Celsjusza.
```

## Deep Dive (Dogłębna analiza)
Choć Arduino nie posiada wbudowanej funkcji interpolacji ciągów jak w językach wyższego poziomu (np. Python), 'sprintf' jest wszechstronnym narzędziem służącym do formatowania ciągów. Po raz pierwszy pojawiło się w języku C i jest często używane w systemach wbudowanych dla zarządzania pamięcią. Alternatywnie, możemy łączyć ciągi za pomocą `+`, ale jest to mniej wydajne pamięciowo. Implementacja `sprintf` na platformach Arduino nie obsługuje zmiennych typu `float` domyślnie, więc dla wartości zmiennoprzecinkowych trzeba korzystać z `dtostrf`.

## See Also (Zobacz również)
- [Dokumentacja Arduino - String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Tutorial: Managing Memory](https://learn.adafruit.com/memories-of-an-arduino)
