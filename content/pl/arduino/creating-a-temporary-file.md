---
title:                "Arduino: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto tworzyć tymczasowe pliki w programowaniu Arduino?
 Tworzenie tymczasowych plików jest niezbędne w programowaniu Arduino, gdyż pozwala nam na tymczasowe przechowywanie danych lub wyników obliczeń. W ten sposób możemy uniknąć zapełnienia pamięci urządzenia, co może spowodować problemy z działaniem naszego projektu.

## Jak to zrobić?

Tworzenie tymczasowego pliku w programowaniu Arduino jest bardzo proste. Możemy to zrobić przy użyciu funkcji ```ArduinoFile::createTempFile()```. Kod poniżej przedstawia przykładowe użycie tej funkcji:

```Arduino
#include <Arduino.h>
#include <SPI.h>
#include <SD.h>

File tempFile; //definiowanie zmiennej dla tymczasowego pliku

void setup() {
  Serial.begin(9600);
  SD.begin(10); //inicjalizacja karty SD na pinie 10
}

void loop() {
  if (SD.exists("dane.txt")) { //sprawdzanie czy plik istnieje na karcie SD
    File myFile = SD.open("dane.txt"); //odczyt pliku 
    while (myFile.available()) { 
      Serial.write(myFile.read()); //wypisanie odczytanego pliku na monitor szeregowy
    }
    myFile.close(); //zamykanie pliku
  } else {
    Serial.println("Nie znaleziono pliku.");
  }
  delay(1000);

  //tworzenie tymczasowego pliku i wypisanie danych
  tempFile = SD.open("temp.txt", FILE_WRITE); //otwieranie tymczasowego pliku w trybie zapisu
  if (tempFile) {
    byte data = 1;
    tempFile.println("Wartość zmiennej data wynosi: " + String(data)); //zapisanie wartości zmiennej do pliku
    tempFile.close(); //zamykanie pliku
  }
  else {
    Serial.println("Błąd tworzenia pliku.");
  }
}
```

W wyniku uruchomienia powyższego kodu na monitorze szeregowym pojawią się dane z pliku "dane.txt", a także komunikat o utworzeniu tymczasowego pliku "temp.txt" i zapisane w nim dane.

## Deep Dive
Tworzenie tymczasowego pliku jest szczególnie przydatne w przypadku, gdy musimy tymczasowo przechować dane lub wyniki obliczeń. Po zakończeniu pracy z plikiem, warto go usunąć przy użyciu funkcji ```ArduinoFile::remove()```.

W przypadku gdy projekt wymaga częstego tworzenia i usuwania tymczasowych plików, warto także zastosować funkcje ```ArduinoFile::rewind()``` i ```ArduinoFile::seek()```, które pozwalają na szybsze i bardziej efektywne operacje na pliku.

Podsumowując, tworzenie tymczasowych plików jest ważnym elementem w programowaniu Arduino, który pozwala na przechowywanie danych i optymalizację wykorzystania pamięci urządzenia.

## Zobacz także
- Dokumentacja funkcji createTempFile(): https://www.arduino.cc/reference/en/libraries/arduino-sdio/arduinofile/createtempfile/
- Przykładowy projekt wykorzystujący tworzenie tymczasowych plików: https://create.arduino.cc/projecthub/robertb/vumetertemp-a8c86f
- Wideo opisujące tworzenie i używanie tymczasowych plików w programowaniu Arduino: https://www.youtube.com/watch?v=BywZpMtkx8U&ab_channel=LearnRobotics