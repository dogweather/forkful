---
title:                "Wysyłanie żądania HTTP"
aliases: - /pl/arduino/sending-an-http-request.md
date:                  2024-01-20T17:58:54.378016-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
HTTP request, czyli żądanie HTTP, to prośba o dane z sieci. Programiści wykorzystują je do zdobycia danych ze stron internetowych lub serwerów, co jest kluczowe w projektach IoT czy interakcjach sieciowych.

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
