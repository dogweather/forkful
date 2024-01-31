---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

**YAML** to format reprezentacji danych, czytelny dla ludzi, używany w konfiguracji oprogramowania i innych miejscach, gdzie wymagana jest prostota. Programiści używają YAML, bo jest prosty w zapisie i czytelny, co ułatwia zarządzanie konfiguracją.

## Jak to zrobić:

Arduino nie obsługuje bezpośrednio YAML, ale możemy skorzystać z bibliotek serwerowych do przetwarzania YAML. Poniżej znajduje się przykład z wykorzystaniem biblioteki `ArduinoJson`, która obsługuje format JSON, bardziej typowy dla Arduino.

```Arduino
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  
  // Przykładowy JSON, który często służy jako alternatywa dla YAML
  const char* json = "{\"temperature\": 23, \"humidity\": 60}";

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, json);

  // Sprawdź czy nie ma błędów
  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  int temperature = doc["temperature"]; // 23
  int humidity = doc["humidity"]; // 60

  Serial.print("Temperatura: ");
  Serial.println(temperature);
  Serial.print("Wilgotność: ");
  Serial.println(humidity);
}

void loop() {
  // Tutaj nic nie robimy.
}
```

Output na Serial Monitor gdy uruchomimy powyższy kod:

```
Temperatura: 23
Wilgotność: 60
```

## Wgłębienie się:

YAML, czyli "YAML Ain't Markup Language", powstał jako alternatywa dla XML i JSON, oferując łatwość w czytaniu i prostotę. W środowisku Arduino, ze względu na ograniczone zasoby, YAML jest rzadziej używany, a programiści częściej wybierają JSON z biblioteką `ArduinoJson`. Jednak przy interakcji z zewnętrznymi systemami lub API, które używają YAML, może być konieczne przetworzenie tego formatu.

## Zobacz również:

- Dokumentacja ArduinoJson: https://arduinojson.org/
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Tutorial JSON w Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/Json
