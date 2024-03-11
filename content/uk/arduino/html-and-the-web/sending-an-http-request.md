---
date: 2024-01-20 17:59:17.129892-07:00
description: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP \u0437\u0430\u043F\u0438\u0442\u0443 - \u0446\u0435 \u0441\u043F\u043E\u0441\
  \u0456\u0431 \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u0457 Arduino \u0437\
  \ \u0432\u0435\u0431\u043E\u043C. \u041F\u0435\u0440\u0435\u0434\u0430\u0447\u0430\
  \ \u0434\u0430\u043D\u0438\u0445 \u0447\u0438 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457\
  \ \u0437 \u0441\u0435\u0440\u0432\u0435\u0440\u0430. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435 \u0434\u043B\u044F \u0434\u0438\u0441\u0442\u0430\u043D\u0446\u0456\
  \u0439\u043D\u043E\u0433\u043E\u2026"
lastmod: '2024-03-11T00:14:23.585181-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0434\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F\
  \ HTTP \u0437\u0430\u043F\u0438\u0442\u0443 - \u0446\u0435 \u0441\u043F\u043E\u0441\
  \u0456\u0431 \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u0457 Arduino \u0437\
  \ \u0432\u0435\u0431\u043E\u043C. \u041F\u0435\u0440\u0435\u0434\u0430\u0447\u0430\
  \ \u0434\u0430\u043D\u0438\u0445 \u0447\u0438 \u043E\u0442\u0440\u0438\u043C\u0430\
  \u043D\u043D\u044F \u0456\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457\
  \ \u0437 \u0441\u0435\u0440\u0432\u0435\u0440\u0430. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435 \u0434\u043B\u044F \u0434\u0438\u0441\u0442\u0430\u043D\u0446\u0456\
  \u0439\u043D\u043E\u0433\u043E\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
---

{{< edit_this_page >}}

## What & Why? (Що таке та Навіщо?)

Відправлення HTTP запиту - це спосіб взаємодії Arduino з вебом. Передача даних чи отримання інформації з сервера. Програмісти роблять це для дистанційного керування пристроями, збору даних або інтеграції з веб-сервісами.

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
