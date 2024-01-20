---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що й Навіщо? / What & Why?

HTTP-запит - це спосіб, яким комп'ютери спілкуються між собою через Веб. Програмісти використовують це для отримання або відправлення даних до сервера.

## Як це зробити / How to:

 ```Arduino
#include <ESP8266WiFi.h>

const char* ssid     = "your_SSID";
const char* password = "your_PASSWORD";

const char* host = "maker.ifttt.com";

void setup() {
  Serial.begin(115200);
  delay(10);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi connected");  
  Serial.println("IP address: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  WiFiClient client;

  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("connection failed");
    return;
  }

  client.print(String("GET /trigger/event/with/key/your_key") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");

  delay(10);

  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }

  Serial.println();
  Serial.println("closing connection");
}
 ```

## Поглиблено / Deep Dive:

- Історичний контекст: HTTP-запити були введені 1990 року та використовувалися як основний протокол передачі даних у Веб.

- Альтернативи: HTTPS (захищений HTTP), API-запити основані на REST або GraphQL.

- Деталі реалізації: Код Arduino використовує ESP8266WiFi-бібліотеку для роботи з Wi-Fi та надсилання GET-запиту до сервера.

## Дивитись також / See Also:

1. Документація ESP8266WiFi: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
2. Посібник по HTTP-запитам: https://developer.mozilla.org/uk/docs/Web/HTTP/Methods
3. Arduino Home: https://www.arduino.cc/
4. HTTP, REST та GraphQL: https://www.codecademy.com/articles/what-is-rest