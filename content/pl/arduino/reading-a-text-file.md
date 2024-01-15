---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Arduino: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli planujesz pracować z dużymi zestawami danych lub przechowywać informacje w strukturalny sposób, to prawdopodobnie będziesz musiał korzystać z plików tekstowych. W tym artykule pokażę Ci jak odczytywać pliki tekstowe za pomocą Arduino, aby ułatwić Ci pracę z danymi.

## Jak to zrobić

Odczytywanie plików tekstowych jest stosunkowo proste w Arduino, dzięki wbudowanej funkcji `File`. W poniższym przykładzie, wykorzystamy tę funkcję do odczytania i wyświetlenia zawartości pliku tekstowego na monitorze szeregowym:

```Arduino
#include <SPI.h>
#include <SD.h>

File dataFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // czekaj na połączenie
  }

  Serial.print("Inicjalizacja karty SD...");

  if (!SD.begin(4)) {
    Serial.println("Błąd!");
    return;
  }
  Serial.println("Gotowe!");
  
  dataFile = SD.open("dane.txt"); // otwórz plik tekstowy
  if (dataFile) {
    // odczytaj i wyświetl zawartość pliku
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close(); // zamknij plik
  }
}

void loop() {
  // pętla główna
}
```

Po wgraniu tego kodu do Arduino i podłączeniu karty SD, możesz zobaczyć zawartość pliku tekstowego na monitorze szeregowym.

## Deep Dive

Funkcja `File` jest częścią biblioteki `SD.h`, która pozwala na zarządzanie kartami SD w Arduino. Aby móc jej używać, musimy najpierw odpowiednio ją zainicjować za pomocą funkcji `SD.begin()`. Następnie, możemy otworzyć dysk z kartą SD za pomocą funkcji `SD.open()`, podając jako argument nazwę pliku, który chcemy odczytać.

Funkcja `File` posiada wiele metod, które ułatwiają pracę z plikami tekstowymi, takich jak `read()`, `available()` czy `close()`. Szczegółowe informacje o tych metodach możesz znaleźć w dokumentacji biblioteki.

Pamiętaj, że plik tekstowy musi znajdować się na karcie SD i musi być podłączona do modułu SD w Arduino. W przeciwnym razie, funkcja `SD.open()` zwróci błąd.

## Zobacz także

Sprawdź poniższe linki, aby dowiedzieć się więcej o odczytywaniu plików tekstowych w Arduino:

- Dokumentacja biblioteki `SD.h`: https://www.arduino.cc/en/reference/SD
- Przykłady i tutoriale: https://www.arduino.cc/en/Tutorial/Files