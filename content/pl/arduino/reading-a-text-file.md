---
title:                "Arduino: Odczytywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie Arduino to fascynujące zajęcie, które daje możliwość tworzenia różnych projektów, od automatycznych systemów nawadniających po inteligentne zabawki. Jednym z ważnych elementów tej platformy jest umiejętność czytania plików tekstowych. Dlatego właśnie warto poznać tę funkcjonalność, aby rozszerzyć swoje możliwości programowania.

## Jak to zrobić

W Arduino dostępne są kilka funkcji, które pozwalają na odczyt danych z plików tekstowych. Jedną z najczęściej używanych jest funkcja `Serial.readString()`, która umożliwia odczyt tekstu wysyłanego na port szeregowy przez komputer. Poniżej znajduje się przykładowy kod, który pokazuje jak użyć tej funkcji:

```Arduino
String inputString = "";  // zmienna do przechowywania odczytanego tekstu

void setup() {
  Serial.begin(9600);  // inicjalizacja portu szeregowego
}

void loop() {
  while (Serial.available() > 0) { // sprawdzenie czy są dostępne dane
    char incomingChar = Serial.read(); // odczyt pojedynczego znaku
    inputString += incomingChar; // dodanie odczytanego znaku do zmiennej inputString
    delay(5); // odczekanie 5 milisekund dla stabilności
  }
  Serial.println(inputString); // wypisanie odczytanego tekstu w monitorze szeregowym
}
```

W tym przykładzie, odczytuje on każdy pojedynczy znak i dodaje go do zmiennej `inputString`. Po zakończeniu odczytywania całej linii, funkcja `Serial.println()` wypisuje odczytany tekst w monitorze szeregowym.

## Głębszy zanurzenie

Arduino oferuje również funkcję `SD.readFile()`, która pozwala na odczyt plików z karty SD. Jest to przydatne, gdy potrzebujemy odczytać większe ilości danych lub gdy chcemy zapisać dane na karcie SD i potem je odczytać. Poniżej znajduje się przykład użycia tej funkcji:

```Arduino
#include <SD.h> // włączenie biblioteki do obsługi karty SD

File myFile; // zmienna dla pliku

void setup() {
  Serial.begin(9600); // inicjalizacja portu szeregowego
  SD.begin(10); // inicjalizacja karty SD na pinie 10
  myFile = SD.open("dane.txt"); // otwarcie pliku o nazwie dane.txt
}

void loop() {
  String inputString = myFile.readString(); // odczyt całego pliku i przypisanie do zmiennej inputString
  Serial.println(inputString); // wypisanie odczytanego tekstu w monitorze szeregowym
}
```

Warto również pamiętać, że przy odczycie pliku z karty SD należy ją wcześniej odpowiednio przygotować, korzystając z funkcji `SD.begin()`.

## Zobacz również

Poniżej kilka przydatnych linków, gdzie można znaleźć więcej informacji o odczycie plików tekstowych w Arduino:

- [Oficjalna dokumentacja Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
- [Poradnik na stronie Instructables](https://www.instructables.com/How-to-Send-Data-From-Arduino-to-Excel-Using-the-SD/)
- [Wideo tutorial na YouTube](https://www.youtube.com/watch?v=fmaVgZ5sNsI)