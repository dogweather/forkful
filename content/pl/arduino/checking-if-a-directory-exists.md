---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Arduino: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, to sprawdzenie czy na naszym dysku istnieje określony katalog. Programiści dokonują tego sprawdzenia, aby uniknąć błędów podczas tworzenia plików lub katalogów.

## Jak to zrobić:

Kiedy mówimy o Arduino, mówimy o mikrokontrolerze, który nie ma systemu plików na własnym pokładzie, więc nie ma typowego "katalogu", jaki znamy z systemów operacyjnych. Jeśli jednak korzystasz z dodatkowej pamięci (jak karty SD), możesz skorzystać z biblioteki SD. Zobacz przykładowy kod poniżej:

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Nie udało się zainicjować karty SD");
    return;
  }

  File root = SD.open("/");

  printDirectory(root, 0);
}

void loop()
{
  //put your main code here, to run repeatedly:

}

void printDirectory(File dir, int numTabs) {
   while(true) {

     File entry =  dir.openNextFile();
     if (! entry) {
       // no more files
       break;
     }
     for (uint8_t i=0; i<numTabs; i++) {
       Serial.print('\t');
     }
     Serial.print(entry.name());
     if (entry.isDirectory()) {
       Serial.println("/");
       printDirectory(entry, numTabs+1);
     } else {
       // files have sizes, directories do not
       Serial.print("\t\t");
       Serial.println(entry.size(), DEC);
     }
     entry.close();
   }
}
```
## W głąb tematu:

Historia tego tematu jest skomplikowana, bo jak wspomnieliśmy - Arduino nie ma na pokładzie typowego systemu plików. Sprawdzanie czy katalog istnieje обecnie jest dostępne dzięki rozwijanym bibliotekom.

Alternatywnie, oprócz biblioteki SD, istnieje wiele innych, takich jak FatFs czy Adafruit SPI Flash. Każda biblioteka ma swoje plusy i minusy oraz różne poziomy obsługi karty SD.

Co do szczegółów implementacji, nasz powyższy kod otwiera katalog główny, a następnie drukuje zawartość katalogu, rekurencyjnie sprawdzając czy są to inne katalogi. Jeśli tak, drukuje ich zawartość. Jeżeli nie, drukujemy nazwę pliku i jego rozmiar.

## Zobacz także:

1. [Dokumentacja biblioteki SD](https://www.arduino.cc/en/Reference/SD)
2. [Biblioteka FatFs](http://elm-chan.org/fsw/ff/00index_e.html)
3. [Biblioteka Adafruit SPI Flash](https://github.com/adafruit/Adafruit_SPIFlash)