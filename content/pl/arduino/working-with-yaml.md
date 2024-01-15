---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś zechciałby pracować z YAML? To oczywiste - składnia YAML jest prostsza i bardziej intuicyjna niż inne formaty, takie jak XML czy JSON, co czyni go idealnym wyborem dla programistów, którzy chcą ułatwić sobie pracę.

## Jak to zrobić

Kodowanie w YAML z użyciem Arduino jest bardzo proste. Wystarczy zaimportować bibliotekę YAML i można zacząć pracować z plikami YAML.

```Arduino
#include <YAML.h>

YAML.begin();

File file = SD.open("plik.yaml");

while (file.available()) {
    String line = file.readString();
    YAML.parse(line);
}

// Odczytanie wartości z pliku YAML
String name = YAML.getValue("name"); // Wartość "name" będzie przechowywana w zmiennej "name"

// Zapisanie wartości do pliku YAML
YAML.setValue("age", "25"); // Ustawienie wartości "age" na 25

YAML.save("nowy_plik.yaml"); // Zapisanie wszystkich zmian do nowego pliku
```

W powyższym przykładzie wykorzystano bibliotekę YAML do otwarcia i odczytania pliku YAML za pomocą Arduino. Następnie wykorzystano funkcje getValue() i setValue() do pobrania i zapisania danych. Na koniec, za pomocą funkcji save(), zmiany zostały zapisane do nowego pliku YAML.

## Głębsza analiza

Plik YAML może zawierać różne typy danych, takie jak liczby, ciągi znaków, listy czy też słowniki. Dzięki temu możemy wygodnie przechowywać i przetwarzać różne informacje w naszych projektach z wykorzystaniem Arudino.

Warto również zwrócić uwagę, że składnia YAML jest wciąż rozwijana i ulepszana, co sprawia, że jest to coraz bardziej popularny format danych w świecie programowania.

## Zobacz także

- [Oficjalna dokumentacja biblioteki YAML dla Arduino](https://arduinojson.org/v6/api/)
- [Tutorial wyjaśniający używanie YAML w Arduino](https://randomnerdtutorials.com/arduino-yaml-parser-arduinojson/)
- [Strona YAML.org - informacje o formacie YAML](https://yaml.org/)