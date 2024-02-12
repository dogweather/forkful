---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- /pl/arduino/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:59.119314-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
W kontekście programowania Arduino, sprawdzenie, czy katalog istnieje na karcie SD lub podobnym module pamięci, pozwala na odczytywanie lub zapisywanie plików bez błędów. Ta operacja jest kluczowa dla logowania danych, zarządzania konfiguracją, lub każdego zadania, które wymaga strukturalnego przechowywania plików, zapewniając niezawodność i płynną wydajność w aplikacjach.

## Jak to zrobić:
Arduino natywnie nie wspiera skomplikowanych operacji na systemie plików od razu po wyjęciu z pudełka. Jednakże, przy użyciu biblioteki SD, która jest częścią standardowego IDE Arduino, możesz łatwo pracować z plikami i katalogami. Aby sprawdzić, czy katalog istnieje, najpierw musisz zainicjować kartę SD, a następnie użyć metody `exists()` z biblioteki SD.

Najpierw dołącz bibliotekę SD i zadeklaruj pin wyboru chipa:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Pin wyboru chipa dla modułu karty SD
```

W funkcji `setup()`, zainicjuj kartę SD i sprawdź, czy katalog istnieje:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Initialization failed!");
    return;
  }

  // Sprawdź, czy katalog istnieje
  if (SD.exists("/myDir")) {
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory doesn't exist.");
  }
}
```
W funkcji `loop()`, możesz ją zostawić pustą lub dodać inne kody operacyjne, w zależności od potrzeb:

```cpp
void loop() {
  // Kod operacyjny lub pozostaw pusty
}
```

Przykładowe wyjście po uruchomieniu kodu będzie albo:

```
Directory exists.
```
lub

```
Directory doesn't exist.
```

Ważne jest, aby upewnić się, że karta SD jest poprawnie sformatowana i że ścieżka katalogu `/myDir` jest zgodna z twoimi konkretnymi potrzebami. Ta podstawowa weryfikacja jest kamieniem węgielnym dla wykonywania bardziej złożonych operacji z plikami i katalogami na kartach SD z Arduino.
