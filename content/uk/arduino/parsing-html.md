---
title:                "Arduino: Розпарсення html."
simple_title:         "Розпарсення html."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Парсинг HTML є важливою та корисною навичкою в програмуванні, особливо для розробки веб-додатків. Це дозволяє отримувати та обробляти дані з веб-сторінок.

## Як це зробити

Для початку вам знадобиться платформа Arduino та декілька необхідних бібліотек. Після цього ви можете виконати наступні кроки:

1. Підключіть до Arduino плату Ethernet або Wi-Fi.
2. Завантажте та встановіть бібліотеку "HTTPClient".
3. Завантажте та встановіть бібліотеку "ArduinoJson".
4. Напишіть код, який створює новий HTTP клієнт та встановлює з'єднання з веб-сторінкою.
5. Визначте URL адресу веб-сторінки та викличте метод `GET` для отримання вмісту.
6. Використовуйте бібліотеку "ArduinoJson" для перетворення отриманого HTML коду в об'єкт JSON для подальшої обробки.

Приведений нижче приклад коду показує, як виконати парсинг HTML на платформі Arduino:

```Arduino
#include <SPI.h>
#include <Ethernet.h>
#include <HTTPClient.h>
#include <ArduinoJson.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // MAC адреса вашої плати Ethernet
IPAddress server(192, 168, 1, 100); // IP адреса веб-сервера
EthernetClient client;

void setup() {
    // Ініціалізація з'єднання з мережею
    Ethernet.begin(mac);
    Serial.begin(9600);
    
    // Створення нового HTTP клієнта та встановлення з'єднання з сервером
    HTTPClient http;
    http.begin(client, server, 80);
    
    // Отримання вмісту з веб-сторінки та збереження у змінну
    String response = http.getString();
    
    // Використовуйте бібліотеку "ArduinoJson" для перетворення HTML коду в об'єкт JSON
    DynamicJsonDocument doc(1024);
    deserializeJson(doc, response);
    
    // Виконайте необхідні дії з отриманим JSON об'єктом
    // Наприклад, виведіть перший елемент масиву "items"
    Serial.println(doc["items"][0]);
}

void loop() {
    // Залиште пустим
}
```

В результаті ви отримаєте об'єкт JSON, який можна обробляти подальшим чином. Наприклад, ви можете зберегти необхідні дані у змінні для подальшого використання.

## Інглибше

Парсинг HTML може бути складнішим завданням, ніж здатися на перший погляд, оскільки веб-сторінки можуть мати різну структуру та форматування. Тому важливо розуміти основи кодування