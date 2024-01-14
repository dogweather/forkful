---
title:                "Arduino: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy, kto pracował z danymi, wie, jak ważne jest przechowywanie ich w obszernym formacie, takim jak CSV. Jest to skrót od "Comma Separated Values" (wartości oddzielone przecinkiem), a używa się go do przechowywania tabelarycznych danych w plikach tekstowych. Dlatego też, jeśli jesteś zainteresowany programowaniem na Arduino, nauka pracy z CSV może być niezbędnym dodatkiem do Twoich umiejętności.

## Jak to zrobić

Poniżej przedstawiono przykładowy kod, który pozwoli Ci na pracę z CSV na Arduino:

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;
String data;

void setup() {
  Serial.begin(9600);

  // Inicjalizacja karty SD
  if (!SD.begin(4)) {
    Serial.println("Nie można zainicjalizować karty SD");
    return;
  }

  // Odczyt pliku CSV
  myFile = SD.open("dane.csv");

  // Jeśli udało się otworzyć plik
  if (myFile) {
    // Czytaj zawartość pliku linia po linii i wyświetlaj na Serial Monitorze 
    while (myFile.available()) {
      data = myFile.readStringUntil('\n');
      Serial.println(data);
    }
    // Zamknij plik
    myFile.close();
  } else {
    // Jeśli nie udało się otworzyć pliku
    Serial.println("Nie można otworzyć pliku");
  }
}

void loop() {
  // Poczekaj 5 sekund między odczytem pliku
  delay(5000);
}
```

Przykładowy plik CSV może wyglądać następująco:

```text
Imię,Nazwisko,Wiek,Miasto
Adam,Kowalski,25,Warszawa
Maria,Nowak,32,Kraków
Jan,Kowalczyk,18,Poznań
```

Po przetestowaniu powyższego kodu, w Serial Monitorze powinno pojawić się takie wyjście:

```text
Imię,Nazwisko,Wiek,Miasto
Adam,Kowalski,25,Warszawa
Maria,Nowak,32,Kraków
Jan,Kowalczyk,18,Poznań
```

## Głębsze zagadnienia

Jeśli chcesz zgłębić swoją wiedzę na temat pracy z CSV na Arduino, warto zwrócić uwagę na kilka ważnych aspektów:

- Duże pliki CSV mogą powodować problemy z pamięcią, dlatego warto przeanalizować swoje dane i ewentualnie podzielić je na kilka plików mniejszych
- Pamiętaj o dopasowaniu prędkości odczytu danych do prędkości Twojego kodu, aby uniknąć problemów z synchronizacją
- W przypadku braku potrzeby edycji danych w pliku CSV, warto użyć funkcji "readString()" zamiast "readStringUntil()", co pozwoli uniknąć tworzenia dodatkowych tymczasowych zmiennych

Nauka pracy z CSV na Arduino może być przydatną umiejętnością w różnych aplikacjach, na przykład w systemach zapisu i odczytu danych lub urządzeniach pomiarowych.

## Zobacz też

Poniżej znajduje się lista linków, które mogą okazać się przydatne podczas pracy z CSV na Arduino:

- [Oficjalna dokumentacja Arduino o pracy z kartami SD](https://www.arduino.cc/en/Reference/SD)
- [Wideo tutorial na temat pracy z CSV na Arduino](https://www.youtube.com/watch?v=tNqHTH5EBQY)
- [Przykładowy projekt: dokonywanie pomiarów i zapis do pliku CSV](https://github.com/RafaGS/MeasureAndSaveToCSV)