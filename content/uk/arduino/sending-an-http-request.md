---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:17.129892-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/sending-an-http-request.md"
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
