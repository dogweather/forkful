---
title:                "Отправка HTTP-запроса"
aliases: - /ru/arduino/sending-an-http-request.md
date:                  2024-01-29T00:02:48.919771-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса - это способ общения вашего Arduino с вебом, например, когда вы просите сервер отправить обратно некоторые данные. Программисты делают это, чтобы позволить их Arduino взаимодействовать с API, получать веб-контент или общаться с другими интернет-базированными сервисами.

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
