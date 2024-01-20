---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie pliku tekstowego to proces wczytywania danych z pliku do pamięci komputera. Programiści robią to po to, aby manipulować danymi, np. analizować je lub używać do sterowania zachowaniem programu.

## Jak zrobić:
Ścieżka do pliku, którego chcesz przeczytać, jest niezbędna. W Arduino, najpierw musisz umieścić plik na karcie SD i podłączyć kartę do płytki Arduino.

Podstawowy kod do zadeklarowania modułu SD wygląda tak:

```Arduino
#include <SD.h>

File myFile;

void setup()
{
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization Failed!");
    return;
  }
  myFile = SD.open("test.txt");
  if (myFile) {
    Serial.println("test.txt:");
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Error opening test.txt");
  }
}

void loop()
{
  // nothing happens after setup
}
```
Jeśli wszystko pójdzie dobrze, twoje dane z pliku tekstowego zostaną wyświetlone w Monitorze Szeregów.

## W Głębi:

1. Historyczne Kontekst: Arduino zaczął obsługiwać odczyt i zapis danych od wersji 1.0, kiedy to wprowadzono bibliotekę SD.

2. Alternatywy: Inne metody obsługi danych w formie tekstowej obejmują użycie pamięci EEPROM, jednak ma ona ograniczoną przestrzeń i jej wielekrótne nadpisywanie skróci żywotność chipa.

3. Szczegóły Implementacji: W Arduino, dane są buforowane podczas odczytu i zapisu na karcie SD. To oznacza, że dane nie są wczytywane ani zapisywane do karty SD "po jednym bajcie", ale jednym blokiem na raz. To zwiększa prędkość odczytu i zapisu.

## Zobacz też:

- [Oficjalna dokumentacja biblioteki SD Arduino](https://www.arduino.cc/en/Reference/SD)
- [Jak używać modułu karty SD z Arduino](https://www.makerguides.com/sd-card-arduino-tutorial/)
- [Czytanie i Zapis plików tekstowych za pomocą Arduino](https://startingelectronics.org/tutorials/arduino/ethernet-shield-web-server-tutorial/SD-card-IO/)