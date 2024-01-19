---
title:                "Інтерполяція рядка"
html_title:           "Arduino: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому? 
Інтерполяція рядка - це процес заміни певних значень в рядку на вказівний текст або змінну. Програмісти використовують цей метод для створення динамічних рядків, які можуть змінюватися залежно від певних умов або значень.

## Як: ```Arduino...```
Існує кілька способів здійснити інтерполяцію рядка в Arduino. Розглянемо два найпоширеніші варіанти, використовуючи змінну "name" і значення "age".

### Використання функції ```print()```
```Arduino
String name = "John";
int age = 30;
print("Вітаємо, " + name + "! Тобі вже " + age + "років.");
```

### Використання функції ```println()```
```Arduino
String name = "John";
int age = 30;
println("Вітаємо, " + name + "! Тобі вже " + age + "років.");
```

Обидва ці приклади використовують операцію додавання (+) для об'єднання рядків та змінних у новий рядок. Результат у першому випадку буде виведений у одному рядку, а у другому - у двох. 

## Глибина дослідження
Інтерполяція рядка не є новим поняттям у програмуванні. Вона була впроваджена у багатьох мовах програмування, включаючи C++, Java та JavaScript. 

Існують інші способи інтерполяції рядка, наприклад, застосування функції ```sprintf()```. Ця функція дозволяє вирівнювати значення та використовувати формати, такі як деспізиторій, десяткове та шістнадцяткове число.

У Arduino використовується тип змінної ```String```, який дозволяє об'єднувати рядки, але обробка великої кількості рядків може призвести до зайвого використання пам'яті.

## Дивись також
Для більш детальної інформації про інтерполяцію рядка в Arduino, рекомендуємо переглянути документацію на офіційному сайті: https://www.arduino.cc/reference/en/language/functions/communication/string-functions/sprintf/