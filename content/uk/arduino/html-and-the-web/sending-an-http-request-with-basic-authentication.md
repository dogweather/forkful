---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/arduino/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:00:58.993152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?
Спрощено кажучи, відправлення HTTP запиту з базовою автентифікацією - це процес використання імені користувача та паролю для доступу до захищеного ресурсу. Програмісти роблять це, щоб забезпечити безпечний обмін даними між пристроєм та сервером.

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
