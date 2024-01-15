---
title:                "Перетворення рядка у нижній регістр"
html_title:           "Arduino: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Чому

Конвертування рядка до нижнього регістру може бути корисно в програмах, які вимагають текст, який складається лише з нижніх літер, або для порівняння двох рядків, ігноруючи регістр літер.

# Як

Для конвертування рядків до нижнього регістру можна використовувати функцію `toLowerCase()` в Arduino. Наприклад:

```Arduino
String myString = "Привіт, Світе!";
myString.toLowerCase(); // результау: "привіт, світе!"
Serial.println(myString); // виводиться: "привіт, світе!"
```

# Deep Dive

Окрім функції `toLowerCase()`, є ще кілька інших способів конвертування рядків до нижнього регістру в Arduino. Одним з них є використання бібліотеки `String` та методу `toLowerCase()`, який приймає як аргумент рядок. Наприклад:

```Arduino
String myString = "Привіт, Світе!";
myString.toLowerCase(myString); // результау: "привіт, світе!"
Serial.println(myString); // виводиться: "привіт, світе!"
```
Іншим способом є використання циклу для проходження через кожен символ рядка та заміни його на відповідний символ в нижньому регістрі, використовуючи функцію `toLowerCase()` з бібліотеки `String`. Наприклад:

```Arduino
String myString = "Привіт, Світе!";
String result = "";

for (int i = 0; i < myString.length(); i++) {
  char c = myString.charAt(i);
  result += String(c).toLowerCase();
}

Serial.println(result); // виводиться: "привіт, світе!"
```

# Дивись Також

- [Документація Arduino про функцію `toLowerCase()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [Документація Arduino про бібліотеку `String`](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Приклади використання функції `toLowerCase()` в Arduino](https://www.electronicshub.org/arduino-string-function/)