---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:54.481512-07:00
description: "Jak to zrobi\u0107: Aby pracowa\u0107 z JSON w Arduino, biblioteka `ArduinoJson`\
  \ jest popularnym wyborem ze wzgl\u0119du na jej \u0142atwo\u015B\u0107 u\u017C\
  ycia i efektywno\u015B\u0107. Umo\u017Cliwia\u2026"
lastmod: '2024-03-13T22:44:35.692804-06:00'
model: gpt-4-0125-preview
summary: "Aby pracowa\u0107 z JSON w Arduino, biblioteka `ArduinoJson` jest popularnym\
  \ wyborem ze wzgl\u0119du na jej \u0142atwo\u015B\u0107 u\u017Cycia i efektywno\u015B\
  \u0107."
title: Praca z JSON
weight: 38
---

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
