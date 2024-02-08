---
title:                "Разбор HTML"
aliases:
- ru/arduino/parsing-html.md
date:                  2024-01-29T00:00:13.060088-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
