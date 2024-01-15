---
title:                "Praca z formatem json"
html_title:           "Arduino: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-json.md"
---

{{< edit_this_page >}}

# Dlaczego JSON jest ważnym elementem programowania Arduino

Jeśli pracujesz z Arduino, prawdopodobnie słyszałeś o JSON (JavaScript Object Notation). Jest to format przechowywania i udostępniania danych, który jest bardzo powszechny w dzisiejszych aplikacjach internetowych. Może to być niezwykle przydatne narzędzie w programowaniu na Arduino, umożliwiające łatwe przetwarzanie i przechowywanie informacji.

## Jak używać JSON w Arduino

Arduino posiada wbudowaną bibliotekę JSON, dzięki czemu nie musisz jej pobierać z zewnętrznych źródeł. Aby zacząć pracę z JSON, musisz najpierw zdefiniować obiekt JSON za pomocą odpowiedniej struktury, w której znajdują się klucze (pola) oraz wartości. Poniżej przedstawione są przykładowe kodowane i wypisywane dane w formacie JSON.

```Arduino
#include <ArduinoJson.h>

void setup() {
  // Definiowanie obiektu JSON
  StaticJsonDocument<200> doc;

  // Ustawianie wartości dla kluczy
  doc["imie"] = "Anna";
  doc["wiek"] = 30;
  doc["hobby"] = "Programowanie";

  // Konwertowanie na dane JSON i wypisanie ich na Serial Monitor
  Serial.begin(9600);
  serializeJson(doc, Serial);
}

void loop() {

}
```

Po wypisaniu danych na Serial Monitor, powinieneś uzyskać następujący wynik:

```Arduino
{"imie":"Anna","wiek":30,"hobby":"Programowanie"}
```

Wykorzystując format JSON, możesz łatwo przekazywać dane do innych urządzeń lub aplikacji, co czyni go niezwykle użytecznym narzędziem w projekcie Arduino.

## Głębsze zagadnienia związane z JSON

Istnieje wiele różnych funkcji związanych z biblioteką JSON, które pozwalają na bardziej zaawansowane manipulowanie danymi. Możesz na przykład odczytywać komunikaty JSON z sieci lub utworzyć bardziej skomplikowany obiekt zagnieżdżonych danych. Jest również możliwość deserializacji danych JSON do obiektów typu String czy int.

Możesz także dostosować rozmiar obiektu JSON, aby zmniejszyć zużycie pamięci. Aby tego dokonać, musisz jedynie zmienić wartość argumentu `StaticJsonDocument` na większą lub mniejszą w zależności od potrzeb.

# Zobacz także

- Dokumentacja biblioteki ArduinoJson: https://arduinojson.org/
- Przewodnik po pracy z JSON w Arduino: https://create.arduino.cc/projecthub/Arduino_Genuino/using-json-in-arduino-projects-c21eed