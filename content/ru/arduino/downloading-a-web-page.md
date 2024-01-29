---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:28.015983-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Скачивание веб-страницы означает получение HTML-содержимого из URL, который вы просматриваете. Программисты делают это для извлечения данных, обновления своих гаджетов или просто для использования интернета не только для просмотра видео с котиками.

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
