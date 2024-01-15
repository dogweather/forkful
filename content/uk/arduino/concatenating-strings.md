---
title:                "Злиття рядків"
html_title:           "Arduino: Злиття рядків"
simple_title:         "Злиття рядків"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Згодом при створенні програми на Arduino вам може знадобитися з'єднати кілька рядків тексту в один. Це може бути корисне для відображення повідомлень, створення логів або надсилання даних через з'єднання бездротового модуля. 

## Як це зробити

Для з'єднання рядків тексту в Arduino використовується функція `strcat()`. Для цього необхідно створити дві змінні типу `char`, які будуть містити текст, який буде з'єднуватися. Потім за допомогою функції `strcat()` з'єднуємо їх та записуємо результат в нову змінну. Нижче наведений приклад коду і його вихідного значення.

```Arduino
char text1[] = "Це";
char text2[] = "Arduino";
char result[13];

strcat(result, text1);
strcat(result, text2);

Serial.println(result); // Вивід: ЦеArduino
```

## Поглиблене вивчення

Крім функції `strcat()`, в Arduino також є інші функції для роботи з рядками, такі як `strcpy()`, `strncpy()` і `strlen()`. Крім того, ви можете додавати не тільки змінні типу `char`, але й числа за допомогою функції `String()`. Нижче наведені додаткові приклади коду і їх вихідних значень.

```Arduino
char text[] = "Arduino ";
int num = 101;
String numString = String(num);

// З'єднання рядка та числа
Serial.println("Hello " + numString); // Вивід: Hello 101

// Використання інших функцій роботи з рядками
strncpy(result, text, 3); // Копіює перші 3 символи із 'text' в 'result'
Serial.println(result); // Вивід: Ard
Serial.println(strlen(result)); // Вивід: 3 (довжина рядка)
```

## Дивись також

- [Документація Arduino про роботу з рядками](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Документація Arduino про функцію `strcat()`](https://www.arduino.cc/reference/en/language/functions/string-manipulation/strcat/)