---
title:    "Arduino: Змінивши регістр рядка"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому
Програмування на Arduino може здатися складною та іноді незрозумілою задачею для багатьох українських користувачів. Однак, навіть найпростіші операції, такі як зміна регістру, можуть бути корисними для розв'язання певних завдань, як наприклад, управління клавішною або діодною LED лампочкою.

## Як це зробити
Використовуючи кілька рядків коду, ви можете створити функцію, яка приймає вхідний рядок та повертає його з капіталізованим першим символом. Нижче ви можете побачити приклад коду та вихідний результат:

``` Arduino
String capitalizingString(String input) {
  // Перевірка чи рядок не пустий
  if (input.length() == 0) {
    return "Порожній рядок!";
  }
  // Отримання першого символу рядка
  char firstChar = input.charAt(0);
  // Перевірка чи символ є буквою
  if (isAlpha(firstChar)) {
    // Перетворення символу на верхній регістр
    firstChar = toupper(firstChar);
    // Повертаємо рядок з першим символом у верхньому регістрі
    return input.charAt(0) + input.substring(1);
  } else {
    return "Перший символ не є буквою!";
  }
}
// Застосування функції для рядка
String inputString = "arduino";
String outputString = capitalizingString(inputString);
// Вивід результату
Serial.print(outputString); // Arduino
```

## Глибинний аналіз
Окрім першого символу, ви також можете капіталізувати усі букви у рядку. Для цього просто потрібно перевірити кожен символ та використати функцію `toupper()` для зміни регістру. Можна також додати умову, щоб ігнорувати символи, які не є буквами.

## Дивіться також
- [Документація Arduino](https://www.arduino.cc/reference/en/language/functions/strings/capitalize/)
- [Відеоурок "Змінюємо регістр рядка в Arduino"](https://www.youtube.com/watch?v=b99J8TqzgkY)
- [Приклади програм для Arduino з капіталізацією рядка](https://github.com/markerzoo/Arduino-String-Capitalization-Examples)