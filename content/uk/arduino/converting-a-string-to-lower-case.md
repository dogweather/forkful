---
title:                "Arduino: Перетворення рядка в нижній регістр"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Конвертування рядка в нижній регістр - це корисна техніка для обробки текстової інформації. Використовуючи це, ви можете легко порівнювати рядки та здійснювати інші операції з текстом.

## Як

```Arduino
String original = "ТЕКСТ В ВЕРХНЬОМУ РЕГІСТРІ";
String lowerCase = original.toLowerCase();

Serial.print(lowerCase); // результат: текст в верхньому регистрі
```

В цьому прикладі ми використали функцію `toLowerCase()` для конвертування рядка `original` в нижній регістр. Результат був виведений на моніторі серійного порту за допомогою функції `Serial.print()`. Таким чином, ми отримали рядок `lowerCase`, який має значення `текст в верхньому регістрі`.

## Глибокий занурення

Конвертація рядка в нижній регістр виконується за допомогою функції `toLowerCase()`, яка є частиною об'єкту `String`. Ця функція змінює кожен символ в рядку на його еквівалент в нижньому регістрі. На відміну від мови C, де пониження регістру виконується за допомогою функції`tolower()`, в Arduino це хибне ім'я функції.

## Дивись також

- [String.toLowerCase() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [String data type tutorial](https://www.arduino.cc/en/Tutorial/String)