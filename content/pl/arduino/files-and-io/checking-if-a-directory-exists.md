---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:59.119314-07:00
description: "Jak to zrobi\u0107: Arduino natywnie nie wspiera skomplikowanych operacji\
  \ na systemie plik\xF3w od razu po wyj\u0119ciu z pude\u0142ka. Jednak\u017Ce, przy\
  \ u\u017Cyciu biblioteki SD,\u2026"
lastmod: '2024-03-13T22:44:35.685845-06:00'
model: gpt-4-0125-preview
summary: "Arduino natywnie nie wspiera skomplikowanych operacji na systemie plik\xF3\
  w od razu po wyj\u0119ciu z pude\u0142ka."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

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
