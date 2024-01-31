---
title:                "Praca z JSON"
date:                  2024-01-19
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON to format wymiany danych, łatwy do odczytu dla ludzi i maszyn. Używamy go, bo świetnie współgra z wieloma językami programowania, w tym z Arduino, do przechowywania konfiguracji czy komunikacji z serwerami.

## Jak to zrobić:
Do obsługi JSON w Arduino użyjemy biblioteki `ArduinoJson`, którą najpierw trzeba zainstalować przez Menadżer Bibliotek w IDE.

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Przykładowy JSON
  const char* json = "{\"temperature\": 23.5}";

  DynamicJsonBuffer jsonBuffer;
  JsonObject& root = jsonBuffer.parseObject(json);

  if (!root.success()) {
    Serial.println("Błąd: nie udało się sparować JSONa");
    return;
  }

  float temperature = root["temperature"];
  Serial.print("Temperatura: ");
  Serial.println(temperature);
}

void loop() {
  // tu nic nie robimy
}
```

Spodziewane wyjście:
```
Temperatura: 23.5
```

## Głębsze spojrzenie:
JSON, JavaScript Object Notation, zyskał popularność w latach 2000. jako alternatywa dla XML. ArduinoJson to nie jedyna opcja – inne biblioteki to na przykład `json-streaming-parser`. Ważne jest rozróżnienie między `DynamicJsonBuffer` a `StaticJsonBuffer` – pierwszy dynamicznie alokuje pamięć, drugi wymaga zadeklarowania rozmiaru z góry.

## Zobacz też:
- Dokumentacja ArduinoJson: https://arduinojson.org/
- Tutorial JSON w Arduino: https://randomnerdtutorials.com/decoding-and-encoding-json-with-arduino-or-esp8266/
- JSON w praktyce: https://www.json.org/json-pl.html
