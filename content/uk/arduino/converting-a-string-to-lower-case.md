---
title:                "Arduino: Переведення рядка в нижній регістр"
simple_title:         "Переведення рядка в нижній регістр"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Чому

Іноді при програмуванні на Arduino виникає потреба у конвертуванні рядка у нижньому регістрі. Це може стати корисним при порівнянні рядків або під час роботи зі змінними, які мають різний регістр.

## Як це зробити

Конвертацію рядка у нижній регістр в Arduino можна виконати за допомогою вбудованих функцій `toLower()` та `toLowerCase()`.

```Arduino
String myString = "ТЕСТОВИЙ РЯДОК";
Serial.println(myString); // виводимо оригінальний рядок
myString.toLowerCase(); // конвертуємо у нижній регістр
Serial.println(myString); // виводимо змінений рядок
```

Результатом виконання буде видно, що перший рядок виводиться з великої літери, а другий - з маленької. 

## Глибше

У деяких випадках, наприклад, при порівнянні рядків, може виникнути необхідність у використанні функції `toLower()`. Вона дозволяє не тільки конвертувати рядок у нижній регістр, але і перетворювати всі літери на мову ASCII.

Простіший приклад використання функції `toLower()`:

```Arduino
String myString = "AbCdEfGhIjKlMnOpQrStUvWxYz";
myString.toLower();
Serial.println(myString); // виводимо змінений рядок
```

Результатом виконання буде "abcdefghijklmnopqrstuvwxyz".

Щоб бути впевненим у конвертації рядка належним чином, можна також використовувати функцію `toLowerCase()`. Ця функція може використовуватися зі спеціальними символами, такими як лапки або крапки, що не враховуються функцією `toLower()`.

```Arduino
String myString = "Тестовий рядок";
myString.toLowerCase();
Serial.println(myString); // виводимо змінений рядок
```

Результатом буде "тестовий рядок", де усі літери зменшені, але спеціальні символи залишаються незмінними.

# Дивіться також

- [Документація з функції toLower()](https://www.arduino.cc/reference/en/language/variables/string/functions/tolower/)
- [Документація з функції toLowerCase()](https://www.arduino.cc/reference/en/language/variables/string/functions/tolowercase/)