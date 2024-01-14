---
title:                "Arduino: Надсилання запиту http з основною автентифікацією"
simple_title:         "Надсилання запиту http з основною автентифікацією"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту з базовою аутентифікацією є важливим кроком у забезпеченні безпеки вашого проекту. Це дає можливість обмінюватися даними з іншими серверами та апаратною частиною.

## Як

Для того, щоб надіслати HTTP запит з базовою аутентифікацією, вам спочатку необхідно налаштувати необхідні бібліотеки на вашому пристрої. Прикладами таких бібліотек є "WiFiClient.h", "ESP8266WiFi.h" та "HTTPClient.h". Далі, вам потрібно підключитися до вашої Wi-Fi мережі і налаштувати основну аутентифікацію, використовуючи ваші облікові дані. Потім виконайте запит за допомогою функцій "begin" і "GET", та отримайте відповідь з сервера. Нижче ми приведені приклад відправки HTTP запиту з базовою аутентифікацією до сервера "example.com" і отримання відповіді на запит.

```arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <HTTPClient.h>

const char* ssid = "YourNetworkName";
const char* password = "YourNetworkPassword";

void setup() {
  Serial.begin(115200);
  delay(100);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to the WiFi network");
}

void loop() {
  // Make HTTP request
  HTTPClient http;

  http.begin("http://example.com");
  http.setAuthorization("username", "password");
  int httpCode = http.GET();

  if (httpCode > 0) { // Check for the returning code
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  }

  http.end(); //Free the resources
  delay(5000); // Make request every 5 seconds
}
```

Виведення що отримується від сервера:

```
200
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
</html>
```

## Глибоке поринання

Основна аутентифікація (Basic Authentication) є простою та широко використовуваною технологією для забезпечення безпеки у мережевих протоколах. Вона використовує відомі нам заголовки "Authorization" та "Realm", які дозволяють серверу перевіряти облікові дані користувачів та надавати доступ до ресурсів. Ця технологія використовує хеш-функції та базову кодування для забезпечення безпеки даних під час обміну.

## Дивіться також

- [ESP8266 WiFiClient Офіційна документація](https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266WiFi/src/WiFiClient.h)
- [HTTPClient Офіційна документація](https://github.com/arduino-libraries/HTTPClient)
- [Простий приклад використання HTTPClient бібліотеки](https://randomnerdt