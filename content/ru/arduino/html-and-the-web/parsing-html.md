---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:13.060088-07:00
description: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u0440\u043E\u0441\u0435\u0438\u0432\u0430\u043D\u0438\
  \u0435 \u043A\u043E\u0434\u0430 HTML \u0434\u043B\u044F \u0438\u0437\u0432\u043B\
  \u0435\u0447\u0435\u043D\u0438\u044F \u043F\u043E\u043B\u0435\u0437\u043D\u044B\u0445\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 - \u043D\u0430\u043F\u0440\u0438\u043C\u0435\
  \u0440, \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\
  \u043B\u0435\u0444\u043E\u043D\u043D\u043E\u0433\u043E \u043D\u043E\u043C\u0435\u0440\
  \u0430 \u0441\u043E \u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043A\u043E\
  \u043D\u0442\u0430\u043A\u0442\u043E\u0432. \u0417\u0430\u0447\u0435\u043C \u044D\
  \u0442\u043E\u2026"
lastmod: '2024-03-13T22:44:45.526822-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0435\u0442 \u043F\u0440\u043E\u0441\u0435\u0438\u0432\u0430\u043D\u0438\
  \u0435 \u043A\u043E\u0434\u0430 HTML \u0434\u043B\u044F \u0438\u0437\u0432\u043B\
  \u0435\u0447\u0435\u043D\u0438\u044F \u043F\u043E\u043B\u0435\u0437\u043D\u044B\u0445\
  \ \u0434\u0430\u043D\u043D\u044B\u0445 - \u043D\u0430\u043F\u0440\u0438\u043C\u0435\
  \u0440, \u0438\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\
  \u043B\u0435\u0444\u043E\u043D\u043D\u043E\u0433\u043E \u043D\u043E\u043C\u0435\u0440\
  \u0430 \u0441\u043E \u0441\u0442\u0440\u0430\u043D\u0438\u0446\u044B \u043A\u043E\
  \u043D\u0442\u0430\u043A\u0442\u043E\u0432."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Что и Почему?
Парсинг HTML означает просеивание кода HTML для извлечения полезных данных - например, извлечение телефонного номера со страницы контактов. Зачем это делать? Для автоматизации сбора данных или взаимодействия со страницами веба из вашего проекта на Arduino.

## Как это сделать:
Arduino изначально не предназначен для работы в сети, но с помощью внешних модулей (например, ESP8266), вы можете подключаться и получать веб-контент. Здесь мы будем извлекать HTML и искать определенный тег:

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>

const char* ssid = "вашSSID";
const char* password = "вашПАРОЛЬ";

const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  WiFiClient client;
  
  if (!client.connect(host, 80)) {
    Serial.println("Не удалось подключиться");
    return;
  }
  
  client.println("GET / HTTP/1.1");
  client.print("Host: ");
  client.println(host);
  client.println("Connection: close");
  client.println();

  while (client.connected() || client.available()) {
    if (client.available()) {
      String line = client.readStringUntil('\n');
      if (line.indexOf("<title>") >= 0) {
        int startIndex = line.indexOf("<title>") + 7;
        int endIndex = line.indexOf("</title>");
        String pageTitle = line.substring(startIndex, endIndex);
        Serial.println(pageTitle);
      }
    }
  }
}

void loop() {
  // Мы запускаем настройку один раз и получаем искомую информацию. Нет необходимости в цикле.
}
```

Пример выходных данных:
```
Пример домена
```

## Глубокое погружение:
Исторически микроконтроллеры, вроде Arduino, не были предназначены для сложных задач, таких как парсинг HTML. Но всё изменилось с появлением модулей, способных работать в сети, и библиотек, обогащающих их возможности.

Ключ к парсингу HTML - манипуляции со строками. Вы ищете шаблоны. Но помните, HTML может быть беспорядочным. Это не так, как JSON с его надёжной структурой. Данный подход подходит для простых задач, но может не сработать, если HTML внезапно изменится.

Альтернативы? Конечно. Если вы серьезно настроены на парсинг, рассмотрите использование более мощного микроконтроллера, совместимого с Arduino, или такого, который может запускать Linux, что открывает доступ к таким инструментам, как Python с библиотеками, предназначенными для веб-скрейпинга.

Простота Arduino - это и плюс, и минус здесь. Вы можете реализовать базовый парсинг без особых сложностей, но если вам нужно обработать сложный HTML или огромные объемы данных, вы выросли из своего Uno.

## Смотрите также:
- [Репозиторий ESP8266 на GitHub](https://github.com/esp8266/Arduino)
- [Библиотека Arduino HttpClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Веб-скрейпинг с Python](https://realpython.com/python-web-scraping-practical-introduction/)
