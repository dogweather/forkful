---
title:                "Завантаження веб-сторінки"
html_title:           "Arduino: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки може бути необхідним для отримання останніх оновлень, даних або інформації в режимі реального часу. Це також може бути корисним для створення зв'язку з іншими пристроями або додатками.

## Як

```arduino
#include <WiFiClient.h> // Бібліотека для з'єднання з Wi-Fi
#include <ESP8266HTTPClient.h> // Бібліотека для взаємодії із веб-сайтами

const char* ssid = "назва_мережі"; // Замініть на своє значення
const char* password = "пароль_мережі"; // Замініть на своє значення

void setup() {
  Serial.begin(9600); // Ініціалізуємо серійний порт для відображення результатів
  WiFi.begin(ssid, password); // Підключення до Wi-Fi мережі

  while (WiFi.status() != WL_CONNECTED) { // Чекаємо на підключення до мережі
    delay(500);
    Serial.println("Підключення до Wi-Fi...");
  }
  Serial.println("Підключення до Wi-Fi та інтернету успішне!");

  // Вказуємо URL адресу веб-сторінки
  HTTPClient http;
  http.begin("https://example.com");
  
  // Робимо запит на веб-сторінку та зберігаємо результат у змінну
  int httpCode = http.GET();
  String response = http.getString();

  // Виводимо результат у консоль
  Serial.println(httpCode); // Код статусу запиту
  Serial.println(response); // Відповідь на запит

  // Зупиняємо з'єднання
  http.end();
}

void loop() {
  // Ваш код тут
}
```

## Deep Dive

Для завантаження веб-сторінки на Arduino ми використовуємо бібліотеку ESP8266HTTPClient, яка пропонує широкий функціонал для роботи з веб-сайтами. Метод `begin()` використовується для вказівки URL адресу, а метод `GET()` здійснює запит та повертає статус запиту. У випадку успішного запиту, метод `getString()` дозволяє отримати відповідь у вигляді рядка. Також можна використовувати методи `POST()` та `PUT()` для відправки даних на веб-сайт.

## Дивіться також

- [ESP8266HTTPClient documentation](https://github.com/esp8266/Arduino/blob/master/libraries/ESP8266HTTPClient/src/ESP8266HTTPClient.h)
- [Arduino Wi-FiClient Library](https://www.arduino.cc/en/Reference/WiFiClient)