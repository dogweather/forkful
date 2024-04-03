---
date: 2024-01-20 18:00:58.993152-07:00
description: "\u0421\u043F\u0440\u043E\u0449\u0435\u043D\u043E \u043A\u0430\u0436\u0443\
  \u0447\u0438, \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\
  \u044F HTTP \u0437\u0430\u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\
  \u0432\u043E\u044E \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0454\u044E - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0456\u043C\u0435\
  \u043D\u0456 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430\
  \ \u0442\u0430 \u043F\u0430\u0440\u043E\u043B\u044E \u0434\u043B\u044F \u0434\u043E\
  \u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0437\u0430\u0445\u0438\u0449\u0435\
  \u043D\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:49.719670-06:00'
model: gpt-4-1106-preview
summary: "\u0421\u043F\u0440\u043E\u0449\u0435\u043D\u043E \u043A\u0430\u0436\u0443\
  \u0447\u0438, \u0432\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\
  \u044F HTTP \u0437\u0430\u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\
  \u0432\u043E\u044E \u0430\u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\
  \u0446\u0456\u0454\u044E - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0456\u043C\u0435\
  \u043D\u0456 \u043A\u043E\u0440\u0438\u0441\u0442\u0443\u0432\u0430\u0447\u0430\
  \ \u0442\u0430 \u043F\u0430\u0440\u043E\u043B\u044E \u0434\u043B\u044F \u0434\u043E\
  \u0441\u0442\u0443\u043F\u0443 \u0434\u043E \u0437\u0430\u0445\u0438\u0449\u0435\
  \u043D\u043E\u0433\u043E \u0440\u0435\u0441\u0443\u0440\u0441\u0443."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як робити:
Ось приклад коду для відправлення HTTP запиту з Arduino з використанням базової автентифікації:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "YOUR_WIFI_SSID";
const char* password = "YOUR_WIFI_PASSWORD";
const char* host = "your.server.com";
const int httpPort = 80;

// Замініть на свої облікові дані
const char* httpUsername = "admin";
const char* httpPassword = "admin";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi...");
  }

  Serial.println("Connected to WiFi");
  // Створення автентифікації у вигляді base64
  String authHeader = "Basic " + base64::encode(String(httpUsername) + ":" + String(httpPassword));

  // Підключення до сервера
  WiFiClient client;
  if (!client.connect(host, httpPort)) {
    Serial.println("Connection failed");
    return;
  }

  // Відправлення HTTP запиту
  client.println("GET /protected/resource HTTP/1.1");
  client.println("Host: " + String(host));
  client.println("Authorization: " + authHeader);
  client.println("Connection: close");
  client.println();

  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      Serial.println("Headers received, body started");
      break;
    }
  }

  // Читання відповіді сервера
  String serverResponse = client.readString();
  Serial.println("Server response: ");
  Serial.println(serverResponse);
}

void loop() {
  // nothing here
}
```

## Поглиблено:
Відправлення запитів з базовою автентифікацією коріннями сягає донапінтернетової епохи, коли користувачів потрібно було аутентифікувати простим і безпечним шляхом. Хоча базова автентифікація не є найбезпечнішим методом (адже облікові дані кодуються у вигляді Base64, що легко розшифровується), вона проста у використанні та підходить для прототипів та проектів з обмеженим ризиком. Альтернативами є OAuth, токени API та інше. Специфічно в контексті Arduino, важливо зважати на обмежений ресурс пристрою, коли вибираєте метод автентифікації.

## Дивіться також:
- [ESP8266WiFi documentation](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html) - документація по бібліотеці, яку ми використовуємо для підключення до WiFi.
- [Base64 Arduino Library](https://www.arduino.cc/reference/en/libraries/base64) - бібліотека для кодування та декодування даних у форматі Base64.
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) - ресурс для заглиблення у принципи HTTP автентифікації.
