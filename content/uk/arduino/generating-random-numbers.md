---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:48:40.691180-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Генерація випадкових чисел - це процес створення чисел, які не можна передбачити. У програмуванні для Arduino це важливо для створення відчуття невизначеності, наприклад, у іграх, симуляціях чи вибіркових тестуваннях.

## Як це зробити:
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // Ініціалізуємо генератор випадкових чисел
}

void loop() {
  int randomNumber = random(1, 100); // Генеруємо випадкове число від 1 до 99
  Serial.println(randomNumber); // Виводимо його на серійну консоль
  delay(1000); // Затримка між генераціями
}
```
Приклад виводу:
```
23
78
45
...
```

## Підводимо підсумки:
Arduino використовує функцію `random()`, щоб створювати псевдовипадкові числа. "Псевдо" означає, що числа здаються випадковими, але вони генеруються алгоритмічно, тому вони можуть повторюватися. Запуск `randomSeed()` із унікальним вхідним сигналом, таким як непідключений аналоговий вхід, вносить елемент непередбачуваності. Існують інші методи, такі як використання зовнішніх джерел (сенсорів, часу тощо) для вдосконалення випадковості.

## Див. також:
- [Arduino Reference for randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Arduino Reference for random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
