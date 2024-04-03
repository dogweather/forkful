---
date: 2024-01-20 17:58:54.378016-07:00
description: "Jak to zrobi\u0107: Do wysy\u0142ania \u017C\u0105da\u0144 HTTP na Arduino\
  \ u\u017Cyjemy biblioteki `WiFi.h` dla po\u0142\u0105cze\u0144 Wi-Fi i `HTTPClient.h`\
  \ dla \u017C\u0105da\u0144 HTTP. Oto prosty przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.669241-06:00'
model: gpt-4-1106-preview
summary: "Do wysy\u0142ania \u017C\u0105da\u0144 HTTP na Arduino u\u017Cyjemy biblioteki\
  \ `WiFi.h` dla po\u0142\u0105cze\u0144 Wi-Fi i `HTTPClient.h` dla \u017C\u0105da\u0144\
  \ HTTP."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## Jak to zrobić:
Do wysyłania żądań HTTP na Arduino użyjemy biblioteki `WiFi.h` dla połączeń Wi-Fi i `HTTPClient.h` dla żądań HTTP. Oto prosty przykład:

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

const char* ssid = "Twoje_SSID";
const char* password = "Twoje_Hasło";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Łączenie z WiFi...");
  }

  HTTPClient http;
  http.begin("http://twojastrona.com/data.json"); // URL do żądania
  int httpCode = http.GET(); // Rozpocznij żądanie
  
  if (httpCode > 0) { // Sprawdź, czy odpowiedź jest pozytywna
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Błąd żądania");
  }

  http.end(); // Zakończ połączenie
}

void loop() {
  // Na razie pusty.
}
```
Po uruchomieniu kodu zobaczysz w Serial Monitor odpowiedź serwera (status oraz dane).

## Dogłębna analiza:
W latach 90. HTTP ustanowiło się jako podstawowy protokół komunikacyjny w Internecie. Dziś, żądania HTTP to fundament pobierania danych. 

Alternatywą dla HTTP jest MQTT, powszechnie stosowany w systemach IoT dla lepszego zarządzania siecią i mniejszego zużycia energii.

Podczas implementacji żądania HTTP ważne są: 
1. Ustanowienie połączenia z siecią (przy użyciu Wi-Fi, Ethernet, etc.).
2. Skonfigurowanie klienta HTTP i wykonanie żądania (GET, POST, itd.).
3. Obsługa odpowiedzi, włącznie z kodami błędów i danych.

## Zobacz też:
- Dokumentacja Arduino HTTPClient: https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient
- Więcej o protokole HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Alternatywny protokół – MQTT: https://mqtt.org/
