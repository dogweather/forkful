---
title:                "Arduino: Знаходження довжини рядка"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Чому
Знаходження довжини рядка є важливою функцією в програмуванні на Arduino. Наприклад, ви можете використовувати цю функцію для визначення кількості символів в рядку перед тим, як виводити його на LCD дисплей або для розбивання рядка на окремі частини для подальшої обробки.

# Як це зробити
У кодових блоках "```Arduino ... ```" нижче наведені приклади коду та приклади виводу для знаходження довжини рядка. За допомогою цих прикладів ви зможете швидко і легко засвоїти цю важливу функцію.

```Arduino
// Варіант 1: Використання функції strlen()
char str[] = "Привіт Arduino";
int length = strlen(str);
Serial.println(length); // Виведе 15, тобто кількість символів в рядку

// Варіант 2: Ручний розрахунок
char str[] = "Привіт Arduino";
int length = 0;
for (int i = 0; str[i] != '\0'; i++) {
  length++;
}
Serial.println(length); // Виведе 15, тобто кількість символів в рядку
```

# Глибше в деталі
В першому прикладі використовується вбудована функція `strlen()`, яка повертає кількість символів в рядку, не включаючи символ закінчення рядка `\0`. В другому прикладі ми використовуємо цикл `for` для ручного підрахунку кількості символів в рядку.

Незалежно від вибраного варіанту, важливо пам'ятати, що рядки в Arduino повинні завершуватись символом `\0` для коректної роботи з функцією `strlen()`.

# Дивись також
- [Рядки в Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Функція strlen()](https://www.arduino.cc/reference/uk/language/variables/data-types/string/functions/strlen/)
- [Приклади роботи з рядками в Arduino](https://www.instructables.com/id/Working-With-Strings-in-Arduino/)