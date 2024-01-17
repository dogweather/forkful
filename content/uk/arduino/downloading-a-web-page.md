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

## Що і чому?
Завантаження веб-сторінки - це процес отримання інформації з Інтернету на ваш пристрій. Програмісти часто використовують цей процес, щоб отримати доступ до корисної інформації для своїх проектів або програм.

## Як це зробити:
Нижче наведені приклади коду та результату для завантаження веб-сторінки за допомогою Arduino.

```Arduino
#include <WiFi.h>

void setup() {
  // підключення до Wi-Fi мережі
  WiFi.begin("назва_мережі", "пароль_мережі");
  // очікування підключення
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Підключення до Wi-Fi...");
  }
  
  Serial.println("Підключено до Wi-Fi!");
}

void loop() {
  // створення об'єкту Wi-Fi клієнту
  WiFiClient client;
  // підключення до сервера
  if (client.connect("www.example.com", 80)) {
    Serial.println("Підключено до сервера!");
    // відправка команди GET для отримання веб-сторінки
    client.println("GET / HTTP/1.0");
    // очікування відповіді від сервера
    while (client.available()) {
      // виведення вмісту веб-сторінки в консоль монітора серійного порту
      Serial.write(client.read());
    }
    // розрив з'єднання з сервером
    client.stop();
    Serial.println("З'єднання з сервером закрито!");
  } else {
    Serial.println("Помилка з'єднання!");
  }
  
  delay(5000); // очікування 5 секунд
}
```

Результат виводу в консоль монітора серійного порту:
```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...

<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
...
</body>
</html>
```

## Погляд у глибину:
Історичний контекст: завантаження веб-сторінок є одним із основних функцій Інтернету і постійно вдосконалюється з появою нових технологій і стандартів. Інші альтернативи для програмістів - використання спеціалізованих бібліотек або розробка власних рішень.

Деталі реалізації: для завантаження веб-сторінок за допомогою Arduino, ми використовуємо бібліотеку WiFi.h, яка дозволяє підключатися до Wi-Fi мережі та використовувати стандартні протоколи Інтернету, такі як HTTP для отримання веб-сторінок.

## Також перегляньте:
- Документацію бібліотеки WiFi.h для докладнішої інформації про її використання (https://www.arduino.cc/en/Reference/WiFi)
- Інші способи завантаження веб-сторінок на Arduino (https://randomnerdtutorials.com/esp8266-web-server-arduino-ide/)