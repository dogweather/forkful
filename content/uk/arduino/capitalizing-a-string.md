---
title:                "Arduino: Форматування рядка"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Чому

Цей блог-пост призначений для українських читачів, які цікавляться програмуванням на Arduino. Сьогодні ми говоритимемо про узагальнення (капіталізацію) стрічки, а саме про те, як зробити таке завдання за допомогою Arduino.

# Як виконати

У програмуванні на Arduino, узагальнення стрічки є досить простою задачею. Завдяки функції **toUpperCase** можна легко перетворити всі символи в стрічці на великі літери.

```Arduino

void setup() {
  String text = "привіт світ!";
  text.toUpperCase();
  Serial.println(text); // виведе "ПРИВІТ СВІТ!"
}

void loop() {

}

```

# Глибше занурення

Але чи знаєте ви, що в програмуванні узагальнення стрічки реалізоване за допомогою ASCII кодів? ASCII код визначає числове значення для кожного символу, щоб комп'ютер міг розуміти його. Наприклад, буква "А" має ASCII код 65, а буква "а" - 97.

Тому, коли ми викликаємо функцію **toUpperCase**, комп'ютер насправді замінює числове значення кожного символу на відповідну велику літеру. Це зроблено за допомогою арифметичних операцій, в яких до числа, що відповідає символу, додається 32 (65 + 32 = 97).

# Дивіться також

- [ASCII код](https://uk.wikipedia.org/wiki/ASCII)
- [Функція toUpperCase](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)