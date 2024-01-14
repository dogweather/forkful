---
title:                "Arduino: Робота з json"
simple_title:         "Робота з json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Для чого працювати з JSON на Arduino? JSON є одним з найбільш популярних форматів обміну даними в мережі Інтернет. Використання JSON дозволяє ефективно передавати та обробляти дані, що дозволяє ефективно робити його для проектів на Arduino.

## Як

Для початку, потрібно включити бібліотеку "ArduinoJson" у своїй програмі. Потім можна вибрати, чи потрібно працювати з JSON об'єктами або масивами. Наприклад, створимо JSON об'єкт за допомогою функції "createObject()" та додамо до нього ключі та значення за допомогою функції "add()". Виводимо результат за допомогою "serializeJson()" та отримуємо наступний результат:

```arduino
#include <ArduinoJson.h>

void setup() {
  // створення JSON об'єкту
  StaticJsonDocument<200> jsonDoc;
  
  // додавання ключів та значень
  jsonDoc["name"] = "John";
  jsonDoc["age"] = 25;
  
  // вивід результату
  serializeJson(jsonDoc, Serial);
}
```

В результаті у моніторі порта з'явиться наступне:

```
{"name":"John","age":25}
```

## Глибокий погляд

Окрім створення та додавання значень до JSON об'єктів, бібліотека "ArduinoJson" також має багато інших функцій, які дозволяють ефективно працювати з даними у форматі JSON. Це може бути корисно при зберіганні та передаванні складних даних, наприклад, даних з датчиків або інформації про стан системи.

Детальну документацію бібліотеки та приклади використання можна знайти на їх офіційному веб-сайті.

## Дивись також

- [Офіційний веб-сайт ArduinoJson](https://arduinojson.org/)
- [Документація по бібліотеці ArduinoJson](https://arduinojson.org/v6/api/jsonobject/createobject/)
- [Приклади використання бібліотеки ArduinoJson](https://github.com/bblanchon/ArduinoJson/tree/master/examples)