---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Arduino: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Nadrzędnym celem jest sprawdzenie, czy dany katalog istnieje w celu uniknięcia błędów podczas operacji z plikami.

## Jak to zrobić:

```Arduino
#include <SD.h>  // Dołączenie biblioteki obsługującej moduł SD

void setup() {
  Serial.begin(9600); // Inicjalizacja portu szeregowego
  if (SD.begin(10)) { // Inicjalizacja modułu SD z przypisaniem pinu do obsługi
    // Sprawdzenie czy katalog istnieje
    if (SD.exists("/test")) { 
      Serial.println("Katalog istnieje!");
    }
    else {
      Serial.println("Katalog nie istnieje!");
    }
  }
  else {
    Serial.println("Błąd inicjalizacji modułu SD!");
  }
}

void loop() {
  // Pusta pętla
}
```

Output:
```
Katalog istnieje!
```

## Deep Dive:

Aby sprawdzić czy dany katalog istnieje, musimy najpierw zainicjalizować moduł obsługujący karty SD za pomocą funkcji SD.begin(). Następnie, używając funkcji SD.exists(), możemy sprawdzić czy dany katalog istnieje w strukturze katalogów. Funkcja ta zwraca wartość true lub false, w zależności od wyniku. Jest to ważne, ponieważ unikamy błędów podczas próby operacji na plikach znajdujących się w nieistniejącym katalogu.

## Zobacz też:

- [Dokumentacja funkcji SD.exists()](https://www.arduino.cc/en/Reference/SDexists)
- [Tutorial o obsłudze kart SD z Arduino](https://www.arduino.cc/en/Tutorial/ArduinoSD)