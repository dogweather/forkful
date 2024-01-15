---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Перевірки довжини рядків можуть бути корисними для багатьох проектів на Arduino, наприклад, для роботи з датчиками або виведення інформації на дисплей. Ця функція дозволяє зручно та швидко отримувати необхідну інформацію про довжину рядків.

## Як

```Arduino
void setup() {
  Serial.begin(9600); // Запуск послідовного з’єднання
  String text = "Hello World!"; // Створення рядка для перевірки
  int length = text.length(); // Пошук довжини рядка
  Serial.print("Довжина рядка: "); 
  Serial.println(length); // Відображення довжини рядка у послідовному вигляді
}

void loop() {

}
```

Після виконання цього коду, у послідовному моніторі з’явиться повідомлення "Довжина рядка: 12". Це означає, що довжина рядка "Hello World!" складає 12 символів.

## Глибокий аналіз

Для пошуку довжини рядка використовується вбудована функція length(). Ця функція повертає значення типу int, тобто ціле число, що відповідає довжині рядка у символах. Варто пам’ятати, що функція length() не враховує символ закінчення рядка, тому фактична довжина може бути на один символ меншою.

## Дивись також

- [Робота з рядками в Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Вбудовані функції Arduino](https://www.arduino.cc/reference/en/#functions)