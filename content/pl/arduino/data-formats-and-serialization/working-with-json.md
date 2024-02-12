---
title:                "Praca z JSON"
date:                  2024-02-03T19:21:54.481512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

JSON, czyli Notacja Obiektów JavaScript, to lekki format wymiany danych, idealny do przechowywania danych lub plików konfiguracyjnych w projektach Arduino. Programiści używają go ze względu na jego prostotę i czytelność w różnych środowiskach programistycznych, w tym Arduino, umożliwiając bezproblemową wymianę danych z interfejsami API sieci web lub innymi systemami.

## Jak to zrobić:

Aby pracować z JSON w Arduino, biblioteka `ArduinoJson` jest popularnym wyborem ze względu na jej łatwość użycia i efektywność. Umożliwia ona parsowanie ciągów JSON, modyfikowanie ich oraz serializację obiektów z powrotem do ciągów JSON. Oto jak jej używać:

1. **Instalacja biblioteki ArduinoJson**: Użyj Menedżera Bibliotek w środowisku Arduino IDE i zainstaluj "ArduinoJson".

2. **Deserializacja ciągu JSON**: Oto jak przetworzyć ciąg JSON i wyodrębnić wartości.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Dostosuj rozmiar zgodnie z dokumentem JSON
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* czujnik = doc["sensor"]; // "gps"
  long czas = doc["time"]; // 1351824120
  float szerokośćGeograficzna = doc["data"][0]; // 48.756080
  float długośćGeograficzna = doc["data"][1]; // 2.302038
  
  Serial.println(czujnik);
  Serial.println(czas);
  Serial.println(szerokośćGeograficzna, 6);
  Serial.println(długośćGeograficzna, 6);
}

void loop() {
  // Pusta pętla
}
```

Przykładowe wyjście:

```
gps
1351824120
48.756080
2.302038
```

3. **Serializacja do ciągu JSON**: Oto jak stworzyć ciąg JSON z danych.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Dostosuj rozmiar zgodnie z danymi
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray dane = doc.createNestedArray("data");
  dane.add(48.756080);
  dane.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Pusta pętla
}
```

Przykładowe wyjście (sformatowane dla czytelności):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Efektywne używanie biblioteki `ArduinoJson` pozwala projektom Arduino na komunikację złożonych struktur danych w formacie czytelnym dla człowieka, ułatwiając rozwój i integrację z usługami webowymi.
