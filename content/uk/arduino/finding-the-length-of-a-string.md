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

## Що та чому?

Знаходження довжини рядка - це процес визначення кількості символів у рядку. Програмісти роблять це, щоб контролювати обробку даних та забезпечувати коректну роботу додатків.

## Як це робити:

Код Arduino, щоб знайти довжину рядка, можна зробити за допомогою функції `length()`, яка повертає кількість символів. Ось простий приклад:

```Arduino
String myString = "Hello, world!";
int length = myString.length();
Serial.println(length);
```
Цей скетч виведе "13" - кількість символів у "Hello, world!".

## Поглиблено

1. **Історичний контекст**: Функція `length()` була введена в Arduino з їх початкової версії і є стандартним методом обработки рядків до сих пір.
   
2. **Альтернативний метод**: Якщо ви не хочете користуватися вбудованою функцією `length()`, ви можете обрахувати довжину рядка вручну, використовуючи цикл:

    ```Arduino
    String myString = "Hello, world!";
    int length = 0;
    for(int i=0; myString[i]; i++)
      length++;
    Serial.println(length);
    ```
3. **Деталі реалізації**: Функція `length()` працює швидко і ефективно, оскільки Arduino зберігає довжину рядка в пам'яті.

## Дивіться також

[Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/): Офіційна документація Arduino по рядкам.  
[Arduino Programming Course](https://startingelectronics.org/software/arduino/learn-to-program-course/): Хороший вступний курс з програмування на Arduino.