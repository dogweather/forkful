---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:28.015983-07:00
description: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ HTML-\u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E \u0438\
  \u0437 URL, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0432\u044B \u043F\u0440\
  \u043E\u0441\u043C\u0430\u0442\u0440\u0438\u0432\u0430\u0435\u0442\u0435. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F \u0438\u0437\u0432\u043B\
  \u0435\u0447\u0435\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445, \u043E\
  \u0431\u043D\u043E\u0432\u043B\u0435\u043D\u0438\u044F\u2026"
lastmod: '2024-03-13T22:44:45.528903-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043A\u0430\u0447\u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\
  \u0431-\u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435\
  \ HTML-\u0441\u043E\u0434\u0435\u0440\u0436\u0438\u043C\u043E\u0433\u043E \u0438\
  \u0437 URL, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0432\u044B \u043F\u0440\
  \u043E\u0441\u043C\u0430\u0442\u0440\u0438\u0432\u0430\u0435\u0442\u0435."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как:
Вот суть: заставьте ваш Arduino серфить по интернету и захватывать то, что вам нужно.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "вашSSID";
const char* password = "вашПАРОЛЬ";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Подключение к WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com"); // Замените на ваш URL
  
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    if (httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("Ошибка в HTTP-запросе: %s\n", http.errorToString(httpCode).c_str());
  }
  http.end();
}

void loop() {
  // Пока здесь ничего нет.
}
```

Включите его, и вы должны увидеть HTML веб-страницы в Мониторе Последовательного Порта. Помните, вам понадобится модуль ESP8266 Wi-Fi и подключение к сети.

## Подробнее
Когда-то Arduinos были простыми оффлайновыми устройствами. Затем появились щиты и модули, соединяющие их с большим злым вебом. ESP8266 – это один из таких волшебных гаджетов, превращающих ваш Arduino в интернет-серфера.

Альтернативы? Конечно, есть. Существуют ESP32, Ethernet Shield и другие устройства для той же задачи.

Качество вашего интернет-соединения, надежность источника питания и даже время суток могут повлиять на эффективность скачивания страницы вашим Arduino. Мы действительно подключаем больше факторов, чем просто написание изящного кода.

## Смотрите также
Хотите узнать больше? Посмотрите здесь:

- [Сетевые возможности Arduino](https://www.arduino.cc/en/Guide/ArduinoEthernetShield)
- [ESP8266 GitHub Wiki](https://github.com/esp8266/Arduino)
- [ESP32 GitHub Репозиторий](https://github.com/espressif/arduino-esp32)
