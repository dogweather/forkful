---
title:                "Praca z json"
html_title:           "Arduino: Praca z json"
simple_title:         "Praca z json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Czym jest JSON i dlaczego jest ważne?
JSON (JavaScript Object Notation) to popularny format danych, który jest stosowany w programowaniu. Służy on do przedstawiania informacji w postaci obiektów i tablic, które są czytelne dla ludzi. Programiści korzystają z JSON, ponieważ jest on łatwy do zrozumienia i przetwarzania przez komputery.

## Jak to zrobić:
Arduino ma wbudowaną bibliotekę do obsługi danych w formacie JSON. Aby zacząć, musisz zaimportować bibliotekę za pomocą polecenia ```#include <ArduinoJson.h>```. Następnie możesz użyć funkcji ```parseJson```, aby przetworzyć dane zapisane w formacie JSON. Poniżej znajduje się przykład kodu z wykorzystaniem tej biblioteki:

```
#include <ArduinoJson.h>

void setup() {
  // Tworzenie przykładowego obiektu JSON
  const char* json = "{\"name\":\"John\", \"age\":30, \"town\":\"New York\"}";

  // Parsowanie danych JSON
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  // Pobranie wartości z obiektu JSON
  const char* name = doc["name"];
  int age = doc["age"];
  const char* town = doc["town"];

  // Wyświetlenie danych na Serial Monitorze
  Serial.begin(9600);
  Serial.println(name);
  Serial.println(age);
  Serial.println(town);
}

void loop() {

}
```

Po przesłaniu tego kodu do płytki Arduino, powinieneś zobaczyć w Serial Monitorze dane o imieniu, wieku i miejscu zamieszkania zapisane w obiekcie JSON.

## Głębsze zanurzenie:
JSON został stworzony w 2001 roku i szybko zyskał popularność jako sposób na wymianę danych między aplikacjami internetowymi. Alternatywami dla formatu JSON są m.in. XML i CSV, ale JSON jest często preferowany ze względu na swoją prostotę i czytelność. W przypadku programowania w Arduino, obsługa danych w formacie JSON jest bardzo przydatna w komunikacji z innymi urządzeniami lub serwerami internetowymi.

## Zobacz również:
- Oficjalna dokumentacja biblioteki ArduinoJson: https://arduinojson.org/
- Przykładowy projekt wykorzystujący dane w formacie JSON: https://create.arduino.cc/projecthub/arduino-projects/arduino-json-example-734671