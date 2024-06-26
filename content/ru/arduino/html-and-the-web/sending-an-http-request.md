---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:48.919771-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0420\u0430\u0431\u043E\u0442\u0430 \u0441 Arduino \u0442\u0440\u0435\
  \u0431\u0443\u0435\u0442 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438\
  \ `WiFiNINA` \u0434\u043B\u044F \u0441\u0435\u0442\u0435\u0432\u044B\u0445 \u0444\
  \u0443\u043D\u043A\u0446\u0438\u0439. \u0412\u043E\u0442 \u043A\u0430\u043A \u043E\
  \u0442\u043F\u0440\u0430\u0432\u0438\u0442\u044C \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439 GET-\u0437\u0430\u043F\u0440\u043E\u0441."
lastmod: '2024-03-13T22:44:45.524603-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 Arduino \u0442\u0440\u0435\u0431\
  \u0443\u0435\u0442 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438\
  \ `WiFiNINA` \u0434\u043B\u044F \u0441\u0435\u0442\u0435\u0432\u044B\u0445 \u0444\
  \u0443\u043D\u043A\u0446\u0438\u0439."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
Работа с Arduino требует библиотеки `WiFiNINA` для сетевых функций. Вот как отправить простой GET-запрос:

```Arduino
#include <WiFiNINA.h>

char ssid[] = "yourNetworkName";       // имя вашей сети SSID
char pass[] = "yourNetworkPass";       // ваш сетевой пароль
int status = WL_IDLE_STATUS;           // статус радио WiFi
char server[] = "example.com";         // сервер, к которому вы хотите подключиться

WiFiClient client;

void setup() {
  Serial.begin(9600);                  // начать серийную отладку
  WiFi.begin(ssid, pass);              // начать подключение WiFi
  while (status != WL_CONNECTED) {     // ожидание подключения:
    status = WiFi.status();
    delay(1000);
  }
  Serial.print("Подключено к ");
  Serial.println(ssid);
}

void loop() {
  if (client.connect(server, 80)) {    // если соединение установлено, отправить запрос:
    client.println("GET / HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();                   // конец запроса
  } else {
    Serial.println("Соединение не удалось"); // если соединение с сервером не установлено:
  }

  while (client.connected()) {         // пока соединение установлено, читать данные:
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }

  if (!client.connected()) {           // если сервер отключился, остановить клиента:
    client.stop();
  }

  delay(10000);                        // подождать десять секунд перед следующей попыткой
}
```

Пример вывода:
```
HTTP/1.1 200 OK
Date: Пн, 23 янв 2023 12:36:47 GMT
Server: Apache/2.4.1 (Unix)
...
```

## Подробнее
Концепция отправки HTTP-запроса с микроконтроллера не всегда была актуальной. В прошлом микроконтроллеры использовались больше для датчиков и взаимодействия с физическим миром. Но с появлением IoT (Интернет вещей) эти устройства начали нуждаться в веб-соединении. Теперь Arduino может использовать библиотеки вроде `WiFiNINA`, для надежного управления этими соединениями.

В зависимости от вашего оборудования существуют альтернативы `WiFiNINA`. Например, библиотека `Ethernet` используется для проводных соединений, в то время как `WiFi101` работает со старыми WiFi-щитами.

С точки зрения реализации, выполнение HTTP-запроса может показаться простым, но рукопожатие, заголовки и методы HTTP (GET, POST и т.д.) являются частью строгого протокола, который позволяет устройствам общаться через веб. Arduino абстрагирует многое из этой сложности, но понимание основ помогает решать проблемы, когда что-то идет не так.

## Смотрите также
- Документация библиотеки Arduino `WiFiNINA`: https://www.arduino.cc/en/Reference/WiFiNINA
- Введение в протокол HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Хаб проектов Arduino для проектов, подключенных к вебу: https://create.arduino.cc/projecthub
