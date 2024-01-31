---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:46.460981-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса с базовой аутентификацией добавляет уровень безопасности за счет требования имени пользователя и пароля. Программисты используют это для доступа к API или веб-сервисам, доступ к которым ограничен только авторизованными пользователями.

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
