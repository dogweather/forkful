---
title:    "Arduino: Odczytywanie pliku tekstowego"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś/łaś, aby Twój projekt z Arduino mógł czytać pliki tekstowe? Może chciałeś/łaś użyć danych z zewnętrznego źródła lub po prostu przekazać instrukcje do swojego urządzenia? W tym wpisie pokażemy Ci, jak to zrobić!

## Jak To Zrobić

```Arduino
#include <SD.h>

File dataFile; // zmienna do przechowywania pliku tekstowego

void setup() {
  // inicjalizacja modułu SD
  SD.begin(4);

  // otwarcie pliku tekstowego o nazwie "dane.txt" w trybie odczytu
  dataFile = SD.open("dane.txt", FILE_READ);

  // sprawdzenie, czy plik istnieje
  if (dataFile) {
    // odczytanie danych z pliku
    while (dataFile.available()) {
      // odczytanie i wyświetlenie kolejnych linii
      Serial.println(dataFile.readStringUntil('\n'));
    }
    // zamknięcie pliku
    dataFile.close();
  }
}

void loop() {
  // kod dla pętli głównej programu
}
```
### Przykładowy plik "dane.txt":

```
Temperatura: 25°C
Wilgotność: 50%
```

### Oczekiwany output:

```
Temperatura: 25°C
Wilgotność: 50%
```

## Deep Dive

Moduł SD w Arduino umożliwia dostęp do pamięci SD i obsługę plików. Funkcja `SD.open()` służy do otwarcia pliku z pamięci, przyjmuje ona dwa parametry: nazwę pliku oraz tryb - w tym przypadku `FILE_READ` dla odczytu. Następnie, za pomocą pętli `while` i funkcji `available()`, sprawdzamy, czy są dostępne kolejne dane w pliku. Służy do tego funkcja `readStringUntil()`, która odczytuje dane do napotkania określonego znaku - w tym przypadku do znaku nowej linii `\n`. Na koniec należy zamknąć plik za pomocą funkcji `close()`.

## Zobacz też

- [Dokumentacja Arduino do obsługi modułu SD](https://www.arduino.cc/en/Reference/SD)
- [Przykładowy projekt z wykorzystaniem czytania pliku tekstowego z pamięci SD](https://create.arduino.cc/projecthub/Andre_Julio/arduino-sd-card-temperature-and-humidity-log-1d616e)