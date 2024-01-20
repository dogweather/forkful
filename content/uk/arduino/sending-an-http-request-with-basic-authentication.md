---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Arduino: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та Навіщо?
Відправка HTTP-запиту з базовою автентифікацією – це процес відправки інформації на сервер із верифікацією отримувача. Програмісти використовують це для забезпечення того, що дані спілкуються тільки з хто має на це право.

## Як це зробити:
Ось простий приклад коду для Arduino, який відправляє HTTP-запит з базовою автентифікацією.
```Arduino
#include <ArduinoHttpClient.h>
#include <WiFi101.h>

char ssid[] = "your network";     
char pass[] = "secret password";

int status = WL_IDLE_STATUS;
WiFiClient wifiClient;             
HttpClient client = HttpClient(wifiClient, serverAddress, serverPort);

void setup() {
    WiFi.begin(ssid, pass);
    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
    }
}

void loop() {
    client.beginRequest();
    client.post("/your/endpoint/here");
    client.sendBasicAuth("username", "password");
    client.endRequest();
    delay(10000);
}
```

Тут ми встановлюємо WiFi-з'єднання, створюємо клієнтський об'єкт HttpClient і відправляємо запит кожні 10 секунд.


## Поглиблений Огляд:
Основна автентифікація HTTP виникла на зарі вебу, коли було потрібне просте та легко впроваджуване рішення для захисту ресурсів. Втім, через недоліки з безпекою, багато веб-сервісів переходять на більш безпечні форми автентифікації, наприклад, OAuth2.

Як альтернатива, ви також можете використати HTTPS, який об'єднує HTTP і SSL/TLS протоколи для захищеного з'єднання.

При відправленні HTTP-запиту з базовою автентифікацією, ваші обчислювальні дані (логіни і паролі) кодуються в Base64 без шифрування, що робить їх вразливими для перехоплення.

## Дивіться Також:
1. [Аутентифікація HTTP на Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)
3. [Base64 шифрування на Arduino](https://create.arduino.cc/projecthub/arjun/encoding-decoding-base64-strings-with-arduino-4fad9e)