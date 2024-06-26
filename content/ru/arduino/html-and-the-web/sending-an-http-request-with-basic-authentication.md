---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:46.460981-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0440\u0435\u0430\u043B\u0438\u0437\
  \u043E\u0432\u0430\u0442\u044C \u044D\u0442\u043E \u043D\u0430 Arduino, \u0441\u043D\
  \u0430\u0447\u0430\u043B\u0430 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\
  \u043E \u043F\u043E\u0434\u043A\u043B\u044E\u0447\u0438\u0442\u044C \u043D\u0435\
  \u043E\u0431\u0445\u043E\u0434\u0438\u043C\u044B\u0435 \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0438 \u2013 \u043E\u0431\u044B\u0447\u043D\u043E \u044D\
  \u0442\u043E `<ESP8266WiFi.h>` \u0434\u043B\u044F ESP8266 \u0438\u043B\u0438\u2026"
lastmod: '2024-03-13T22:44:45.530727-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0435\u0430\u043B\u0438\u0437\u043E\
  \u0432\u0430\u0442\u044C \u044D\u0442\u043E \u043D\u0430 Arduino, \u0441\u043D\u0430\
  \u0447\u0430\u043B\u0430 \u043D\u0435\u043E\u0431\u0445\u043E\u0434\u0438\u043C\u043E\
  \ \u043F\u043E\u0434\u043A\u043B\u044E\u0447\u0438\u0442\u044C \u043D\u0435\u043E\
  \u0431\u0445\u043E\u0434\u0438\u043C\u044B\u0435 \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 \u2013 \u043E\u0431\u044B\u0447\u043D\u043E \u044D\u0442\
  \u043E `<ESP8266WiFi.h>` \u0434\u043B\u044F ESP8266 \u0438\u043B\u0438 `<WiFi.h>`\
  \ \u0434\u043B\u044F ESP32 \u0438 `<Base64.h>` \u0434\u043B\u044F \u043A\u043E\u0434\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445\
  \ \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\
  \u0438."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Чтобы реализовать это на Arduino, сначала необходимо подключить необходимые библиотеки – обычно это `<ESP8266WiFi.h>` для ESP8266 или `<WiFi.h>` для ESP32 и `<Base64.h>` для кодирования данных аутентификации. Вот простейший пример для начала:

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid = "yourSSID";
const char* password = "yourPassword";
const char* server = "your.server.com";
const char* authUser = "user";
const char* authPass = "pass";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  
  String auth = "Basic " + base64::encode(String(authUser) + ":" + String(authPass));

  WiFiClient client;
  if (client.connect(server, 80)) {
    client.println("GET /route HTTP/1.1");
    client.print("Host: ");
    client.println(server);
    client.println("Authorization: " + auth);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // Ваш обычный код здесь
}
```

При запуске Arduino подключится к указанному серверу с учетными данными и получит защищенный контент.

## Подробнее
Базовая HTTP аутентификация существует с начала времен интернета, она была определена в 1996 году в RFC 2617. Принцип прост: кодируют имя пользователя и пароль в base64 и добавляют это в HTTP-заголовок. Это не самый безопасный метод (потому что base64 легко обратим), но он простой для малозначительных или внутренних инструментов.

Есть альтернативы, такие как Digest Access Authentication или OAuth, которые более безопасны, но они также требуют больше ресурсов – что стоит учитывать на небольшом Arduino.

При реализации имейте в виду, что кодирование в base64 увеличивает размер учетных данных примерно на 33%, а память Arduino ограничена. Также убедитесь, что ваш сервер использует SSL/TLS (HTTPS), если вы отправляете учетные данные через интернет, чтобы избежать их разглашения.

## Смотрите также
- [Wikipedia о базовой аутентификации](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Обезопасьте свой HTTP-запрос](https://arduino.cc/en/Tutorial/WebClientRepeating)
