---
title:                "Arduino: Sprawdzanie istnienia katalogu"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego codziennego życia. Jeśli jesteś pasjonatem elektroniki i programowania, to z pewnością słyszałeś o Arduino - małej, ale potężnej platformie do tworzenia projektów związanych z automatyką, robotyką czy Internetem rzeczy. Używając Arduino, możemy sprawić, że nasze pomysły ożyją i staną się rzeczywistością. Jedną z ważnych umiejętności w programowaniu Arduino jest sprawdzanie istnienia katalogu. W tym artykule dowiesz się, dlaczego jest to ważne i jak to zrobić.

## Jak to zrobić

Aby sprawdzić, czy katalog istnieje, użyjemy funkcji ```SD.exists()``` wraz z biblioteką SD dla Arduino. Poniższy kod pokazuje, jak to zrobić:

```
#include <SD.h>

// piny używane do komunikacji z czytnikiem kart SD
const int chipSelect = 4;

void setup() {
  // inicjalizacja połączenia z czytnikiem kart SD
  if (!SD.begin(chipSelect)) {
    Serial.println("Nie można znaleźć karty SD");
    return;
  }

  // sprawdzenie, czy katalog "Dane" istnieje
  if (SD.exists("/Dane")) {
    Serial.println("Katalog istnieje!");
  } else {
    Serial.println("Katalog nie istnieje");
  }
}

void loop() {
  // kod wykonywany w pętli
}
```

Po uruchomieniu programu, w monitorze szeregowym powinieneś zobaczyć informację, czy katalog "Dane" istnieje czy nie. Pamiętaj, że aby kod działał poprawnie, musisz mieć podłączoną kartę SD do czytnika.

## Pogłębiona wiedza

Teraz, gdy wiemy, jak sprawdzać istnienie katalogu, warto dowiedzieć się nieco więcej na temat tej funkcji. Funkcja ```SD.exists()``` zwraca wartość logiczną - prawda lub fałsz. Jeśli katalog istnieje, funkcja zwróci prawdę, w przeciwnym wypadku zwróci fałsz. Jest to bardzo przydatne, gdy tworzymy projekty, w których chcemy mieć pewność, że dany katalog lub plik istnieje przed wykonaniem pewnych operacji na nim.

## Zobacz także

- [Dokumentacja funkcji SD.exists()](https://www.arduino.cc/en/Reference/SDExists)
- [Przykładowy projekt z wykorzystaniem funkcji SD.exists()](https://create.arduino.cc/projecthub/Arduino%20Genuino/is-there-a-file-3c17b3)

Nadzieję, że ten artykuł pomógł Ci zrozumieć, dlaczego i jak sprawdzać istnienie katalogu w programowaniu Arduino. Zachęcamy do dalszego samodzielnego eksperymentowania z tą funkcją oraz do tworzenia fascynujących projektów za pomocą Arduino.

**Uwaga:** Aby funkcja ```SD.exists()``` działała poprawnie, musisz mieć zainstalowaną bibliotekę SD. Jeśli nie masz jej na swoim komputerze, możesz ją pobrać z [oficjalnej strony Arduina](https://www.arduino.cc/en/Reference/SD), a następnie dodać ją do swojego projektu w Arduino IDE.