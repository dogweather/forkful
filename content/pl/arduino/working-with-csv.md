---
title:                "Praca z plikami csv"
html_title:           "Arduino: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
CSV (Comma Separated Values) to format pliku, który umożliwia przechowywanie danych w formie tabeli z rozdzielonymi przecinkami. Programiści często używają CSV do przechowywania danych o dużych objętościach, takich jak arkusze kalkulacyjne, ponieważ jest to prosty i wygodny sposób na organizację danych.

## Jak to zrobić:
Możesz łatwo pracować z plikami CSV w Arduino za pomocą biblioteki SoftwareSerial. Najpierw musisz zaimportować bibliotekę, a następnie skonfigurować port szeregowy i prędkość transmisji. Następnie możesz otworzyć plik CSV, odczytać lub zapisać dane, a na koniec zamknąć plik. Poniżej znajduje się przykładowy kod, który pokazuje, jak odczytać dane z pliku CSV i wyświetlić je w monitorze szeregowym.

```Arduino
#include <SoftwareSerial.h> //zaimportuj bibliotekę
#define RX 10 //ustaw RX jako pin 10
#define TX 11 //ustaw TX jako pin 11
SoftwareSerial csv(RX,TX); //utwórz obiekt SoftwareSerial
void setup(){  //konfiguracja portu szeregowego i prędkości transmisji
    Serial.begin(9600);
    csv.begin(9600);
}
void loop(){ 
    if(csv.available()){ //jeśli jest dostępny plik CSV
        String data = csv.readStringUntil(','); //odczytaj dane do pierwszego przecinka
        Serial.println(data); //wyświetl dane w monitorze szeregowym
    }
}
```

Przykładowy wynik:
```
Temperatura: 25C, Wilgotność: 50%, Czas: 10:00
Temperatura: 28C, Wilgotność: 60%, Czas: 12:00
Temperatura: 30C, Wilgotność: 55%, Czas: 14:00
```

## Głęboka Płycizna:
Format CSV został stworzony w 1972 roku jako sposób na przechowywanie danych w arkuszach kalkulacyjnych. Alternatywnym sposobem na przechowywanie danych tabelarycznych jest format JSON. Aby pracować z plikami CSV w Arduino, musisz mieć dostęp do portu szeregowego i odpowiedniej prędkości transmisji. Możesz również użyć gotowej biblioteki do obsługi plików CSV, która ułatwi pracę z danymi.

## Zobacz również:
- Dokumentacja biblioteki SoftwareSerial: https://www.arduino.cc/en/Reference/SoftwareSerial
- Przykładowe projekty z wykorzystaniem plików CSV w Arduino: https://create.arduino.cc/projecthub/search?q=csv
- Poradnik o pracowaniu z plikami CSV w Arduino: https://www.electronicshub.org/working-with-csv-files-in-arduino/