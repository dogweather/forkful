---
title:                "Аналіз HTML"
aliases:
- uk/arduino/parsing-html.md
date:                  2024-02-03T19:12:11.592458-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Розбір HTML у проектах Arduino полягає у витягуванні інформації з веб-сторінок. Програмісти роблять це, щоб дати можливість своїм пристроям Arduino взаємодіяти з Інтернетом, збираючи дані з вебсайтів для цілей, що варіюються від домашньої автоматизації до моніторингу навколишнього середовища.

## Як робити:

Розбір HTML на Arduino зазвичай вимагає бібліотек із мінімальним використанням ресурсів через обмежені можливості пристрою. Популярним вибором для веб-скрапінгу та розбору є використання бібліотек `ESP8266HTTPClient` та `ESP8266WiFi` для ESP8266 або їх аналогів для ESP32, зважаючи на їх вроджену підтримку можливостей Wi-Fi та протоколів HTTP. Ось базовий приклад завантаження та розбору HTML, виходячи з того, що ви працюєте з ESP8266 чи ESP32:

Спершу підключіть необхідні бібліотеки:
```cpp
#include <ESP8266WiFi.h> // Для ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Використовуйте аналогічні бібліотеки ESP32, якщо користуєтесь ESP32

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Підключіться до вашої Wi-Fi мережі:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Підключення...");
    }
}
```

Зробіть HTTP запит та розберіть простий шматочок HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Перевіряємо стан підключення Wi-Fi
        HTTPClient http;  //Оголошуємо об'єкт класу HTTPClient

        http.begin("http://example.com");  //Вказуємо призначення запиту
        int httpCode = http.GET();  //Надсилаємо запит

        if (httpCode > 0) { //Перевіряємо код, що повернувся
            String payload = http.getString();   //Отримуємо відповідь запиту
            Serial.println(payload);             //Друкуємо отримані дані

            // Розбираємо конкретну частину, наприклад, витягуючи заголовок з даних
            int titleStart = payload.indexOf("<title>") + 7; // +7, щоб перескочити "<title>" тег
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Заголовок сторінки: ");
            Serial.println(pageTitle);
        }

        http.end();   //Закриваємо з'єднання
    }

    delay(10000); //Робимо запит кожні 10 секунд
}
```

Приклад виводу (виходячи з припущення, що http://example.com має просту структуру HTML):
```
Підключення...
...
Заголовок сторінки: Example Domain
```

Цей приклад демонструє завантаження HTML-сторінки та витягування змісту тега `<title>`. Для більш складного розбору HTML розгляньте використання регулярних виразів (обережно через обмеження пам'яті) або функцій маніпуляції рядками для навігації через структуру HTML. Більш складний розбір може вимагати більш витончених підходів, включаючи специфічні алгоритми розбору, адаптовані до конкретної структури HTML, з якою ви працюєте, оскільки стандартне середовище Arduino не включає вбудованої бібліотеки для розбору HTML.
