---
date: 2024-01-20 17:43:37.177840-07:00
description: "How to: Do pobierania stron potrzebujesz modu\u0142u sieciowego, np.\
  \ ESP8266. Poni\u017Cej znajdziesz prosty kod, kt\xF3ry \u0142\u0105czy si\u0119\
  \ z Wi-Fi, pobiera dane ze strony i\u2026"
lastmod: '2024-03-13T22:44:35.671169-06:00'
model: gpt-4-1106-preview
summary: "Do pobierania stron potrzebujesz modu\u0142u sieciowego, np."
title: Pobieranie strony internetowej
weight: 42
---

## How to:
Do pobierania stron potrzebujesz modułu sieciowego, np. ESP8266. Poniżej znajdziesz prosty kod, który łączy się z Wi-Fi, pobiera dane ze strony i wyświetla je w Serial Monitor.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "TwojaNazwaSieci";
const char* password = "TwojeHaslo";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Łączenie z WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com"); // Adres strony do pobrania
  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  } else {
    Serial.println("Błąd pobierania strony.");
  }

  http.end();
}

void loop() {
  // Tutaj nic nie robimy
}
```

## Deep Dive
Początki pobierania stron sięgają lat 90., gdy internet przeszedł do użytku publicznego. Od tamtego czasu protokoły jak HTTP ewoluowały, a biblioteki i moduły hardware'owe upraszczają dziś ten proces. Alternatywnie, można użyć Ethernet Shield, ale ESP8266 jest tańsze i ma Wi-Fi. Ważne jest, by pamiętać o bezpieczeństwie danych i korzystać z protokołu HTTPS, zwłaszcza przy wrażliwych danych.

## See Also
- Dokumentacja ESP8266: https://arduino-esp8266.readthedocs.io/en/latest/
- Arduino Network Library: https://www.arduino.cc/en/Reference/WiFi
- Wprowadzenie do HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
