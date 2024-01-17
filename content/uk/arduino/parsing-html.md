---
title:                "Аналіз HTML"
html_title:           "Arduino: Аналіз HTML"
simple_title:         "Аналіз HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# Що і чому?

Парсінг HTML - це процес, який дозволяє програмі помічати певні елементи наеб-сторінки і використовувати їх у своєму коді. Це дуже корисно для отримання даних з Інтернету, таких як ціни на товари або прогнози погоди.

# Як це зробити:

```
Arduino
void setup() {
  Serial.begin(9600); // встановлюємо швидкість передачі даних
}

void loop() {
  String html = "<html><body><h1>Hello, world!</h1></body></html>"; // приклад HTML сторінки
  int start = html.indexOf("<h1>"); // шукаємо початок тегу <h1>
  int end = html.indexOf("</h1>"); // шукаємо кінець тегу </h1>
  String result = html.substring(start + 4, end); // вибираємо дані між тегами
  Serial.println(result); // виводимо результат на монітор
  delay(1000); // пауза 1 секунда
}
```

Результат: "Hello, world!" - це те, що виведе ваш Arduino в монітор.

# Глибоке занурення:

Парсінг HTML був розроблений в 1993 році Тімом Бернерсом-Лі. Існують різні бібліотеки, наприклад, `HTMLParser`, які полегшують процес парсінгу. Також існують інші способи отримання даних з Інтернету, наприклад, використання API або сканування QR-кодів. Навіть при наявності бібліотеки, важливо робити перевірку на наявність даних, щоб уникнути помилок у своєму коді.

# Дивись також:

- Документація для бібліотеки `HTMLParser` (https://www.arduino.cc/reference/en/libraries/htmlparser/)
- Робота з API на Arduino (https://www.arduino.cc/reference/en/libraries/two-wire/)
- Приклад парсінгу HTML на Arduino (https://www.electronicwings.com/arduino/html-parsing-in-arduino-uno)