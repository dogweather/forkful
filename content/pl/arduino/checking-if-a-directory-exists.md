---
title:    "Arduino: Weryfikacja istnienia katalogu"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś twórcą projektów z użyciem Arduino, pewnie wiesz, jak ważne jest, aby program był nie tylko wydajny, ale także bezpieczny. Czasami może się zdarzyć, że potrzebujesz sprawdzić, czy określony folder istnieje przed wykonaniem określonych działań. W tym artykule dowiesz się, dlaczego i jak można to zrobić w Arduino.

## Jak to zrobić

Sprawdzenie, czy dany folder istnieje na karcie SD lub w pamięci wewnętrznej Arduino, jest stosunkowo prostym procesem. Wystarczy użyć funkcji `SD.exists()` lub `SPIFFS.exists()`, w zależności od tego, gdzie znajduje się folder, który chcesz sprawdzić. 

```Arduino
#include <SD.h>
#include <SPIFFS.h>

void setup() {
  // inicjalizacja modułu SD lub SPIFFS
  SD.begin(4); // 4 to numer pinu CS dla modułu SD
  SPIFFS.begin();
}

void loop() {

  // sprawdzenie czy folder "dane" istnieje na karcie SD
  if(SD.exists("/dane")) {
    Serial.println("Folder istnieje na karcie SD!");
  } else {
    Serial.println("Nie znaleziono folderu na karcie SD.");
  }

  // sprawdzenie czy folder "dane" istnieje w pamięci wewnętrznej
  if(SPIFFS.exists("/dane")) {
    Serial.println("Folder istnieje w pamięci wewnętrznej!");
  } else {
    Serial.println("Nie znaleziono folderu w pamięci wewnętrznej.");
  }

  // odczekanie 5 sekund przed kolejną pętlą
  delay(5000);
}
```

Przykładowy output:

```
Folder istnieje na karcie SD!
Nie znaleziono folderu w pamięci wewnętrznej.
```

## Głębsza analiza

Funkcje `SD.exists()` i `SPIFFS.exists()` zwracają wartość `true` lub `false` w zależności od tego, czy folder istnieje lub nie. Można również wykorzystać je do sprawdzenia, czy plik lub inny obiekt istnieje w danym folderze. 

Warto również pamiętać, że przy użyciu `SPIFFS` trzeba najpierw zainicjalizować pamięć wewnętrzną przy pomocy `SPIFFS.begin()`, co zostało pokazane w przykładzie powyżej.

## Zobacz również

1. Dokumentacja Arduino: [Sprawdzanie czy plik istnieje](https://www.arduino.cc/en/Reference/SDexists)
2. Instrukcja obsługi modułu SD z Arduino: [Używanie modułu SD z Arduino](https://learn.adafruit.com/adafruit-tutorial-sparkfun-microsd-shield-arduino-library)
3. Wideo tutorial na YouTube: [Jak używać karty SD w projekcie Arduino](https://www.youtube.com/watch?v=4DzMKqZZ9N0)