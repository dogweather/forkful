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

## Чому

Навіщо комусь спробувати розбирати HTML на Ардуіно? Парсинг HTML дозволяє отримати доступ до вмісту сторінки в Інтернеті і використовувати його у своїх проєктах на Ардуіно.

## Як

Для початку, додайте бібліотеку "ESP8266WiFi.h" до свого проєкту та підключіть Ардуіно до Wi-Fi мережі. Потім використовуйте функцію "client.print()" для отримання вмісту HTML сторінки. Наприклад:

```
#include <ESP8266WiFi.h>

WiFiClient client;

void setup() {
  Serial.begin(115200);
  delay(10);
  WiFi.begin("Назва мережі", "Пароль");
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  client.connect("www.example.com", 80);
  client.print("GET / HTTP/1.1\r\n");
  client.print("Host: www.example.com\r\n\r\n");
}

void loop() {
  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```

Цей код з'єднує Ардуіно з вибраною мережею Wi-Fi і виконує запит на вміст головної сторінки сайту "www.example.com". Після цього він виводить вміст сторінки в монітор серійного порту.

## Deep Dive

Щоб усунути зайві символи з тексту HTML, можна використовувати регулярні вирази. Наприклад, для видалення всіх тегів HTML з тексту, можна використовувати такий код:

```
String text = "<html><body>Привіт світ!</body></html>";
text.replaceAll("<.*?>", "");
Serial.print(text);   // Виводить "Привіт світ!"
```

Це лише приклад можливих операцій з текстом HTML, які можна виконати на Ардуіно.

## Дивіться також

- Приклади використання бібліотеки "ESP8266WiFi.h": https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266WiFi/examples
- Документація з регулярних виразів для Ардуіно: https://www.arduino.cc/reference/en/language/functions/communication/client/ 
- Офіційна документація з синтаксису HTML: https://www.w3schools.com/html/