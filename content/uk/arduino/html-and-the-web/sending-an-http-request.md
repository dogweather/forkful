---
date: 2024-01-20 17:59:17.129892-07:00
description: "How to (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438): \u041F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u043F\u0456\u0434\u043A\
  \u043B\u044E\u0447\u0438\u0442\u0438 \u0432\u0430\u0448 Arduino \u0434\u043E \u0456\
  \u043D\u0442\u0435\u0440\u043D\u0435\u0442\u0443 \u0447\u0435\u0440\u0435\u0437\
  \ Ethernet shield \u0430\u0431\u043E Wi-Fi \u043C\u043E\u0434\u0443\u043B\u044C\
  . \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u043A\u043E\u0434\
  \u0443."
lastmod: '2024-03-13T22:44:49.714703-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u043F\u0456\u0434\u043A\
  \u043B\u044E\u0447\u0438\u0442\u0438 \u0432\u0430\u0448 Arduino \u0434\u043E \u0456\
  \u043D\u0442\u0435\u0440\u043D\u0435\u0442\u0443 \u0447\u0435\u0440\u0435\u0437\
  \ Ethernet shield \u0430\u0431\u043E Wi-Fi \u043C\u043E\u0434\u0443\u043B\u044C."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## How to (Як це зробити):
Потрібно підключити ваш Arduino до інтернету через Ethernet shield або Wi-Fi модуль. Ось приклад коду:

```Arduino
#include <SPI.h>
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
IPAddress server(192, 168, 1, 1);

EthernetClient client;

void setup() {
  Ethernet.begin(mac);
  Serial.begin(9600);

  if (client.connect(server, 80)) {
    client.println("GET /path/to/resource HTTP/1.1");
    client.println("Host: 192.168.1.1");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.write(c);
  }

  if (!client.connected()) {
    Serial.println();
    client.stop();
  }
}
```

Результат у Serial Monitor буде виглядом відповіді сервера.

## Deep Dive (Глибший Занурення):
HTTP запити почали свій розвиток ще з 1991 року, коли було створено протокол HTTP. Для Arduino, варіанти включають Ethernet shield, Wi-Fi модулі (ESP8266/ESP32), і навіть GPRS/3G/4G щити. Важливо знати тип запиту: GET для отримання даних, POST для відправлення. Бібліотеки, як `Ethernet.h` та `WiFi.h`, спрощують процес.

## See Also (Дивіться також):
- [Arduino Ethernet Library](https://www.arduino.cc/en/Reference/Ethernet)
- [Arduino WiFi Library](https://www.arduino.cc/en/Reference/WiFi)
- HTTP протокол деталізація: [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP)

Ця інформація допоможе посиленню ваших навичок взаємодії з веб через Arduino.
