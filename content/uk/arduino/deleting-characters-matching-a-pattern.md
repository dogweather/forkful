---
title:                "Видалення символів, що відповідають шаблону."
html_title:           "Arduino: Видалення символів, що відповідають шаблону."
simple_title:         "Видалення символів, що відповідають шаблону."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Однією з корисних можливостей у програмуванні на Arduino є здатність видаляти символи, що відповідають заданому шаблону. Це може бути корисно при обробці великої кількості даних або фільтрації вхідних сигналів. 

## Як

Для початку, нам потрібно включити бібліотеку `String` для роботи зі змінними типу `String`:

```Arduino
#include <String.h>
```

Далі, створимо змінну типу `String` і надамо їй значення рядка, в якому ми хочемо видалити певні символи. Наприклад, ми маємо рядок "Hello World!" і хочемо видалити всі символи "o":

```Arduino
String myString = "Hello World!";
```

Тепер скористаємося функцією `replace()` для видалення символов. Ця функція приймає два аргументи - символ, який ми хочемо видалити, та символ, на який ми хочемо його замінити, в нашому випадку це буде порожній рядок:

```Arduino
myString.replace('o', '');
```

Якщо ми виведемо значення `myString`, ми побачимо результат:

```Arduino
Serial.println(myString);
// Вивід: Hell Wrld!
```

## Глибоке занурення

Функція `replace()` використовується для заміни всіх входжень символу в рядку. Якщо ви хочете видалити лише перше входження символу, ви можете використовувати функцію `replaceFirst()`. Крім того, ви можете видаляти не тільки один символ, але й цілий підрядок за допомогою функції `remove()`. 

Наприклад, ми маємо рядок "This is a sentence." і хочемо видалити слово "a":

```Arduino
String myString = "This is a sentence.";
myString.remove("a ");
```

Результат буде наступним:

```Arduino
Serial.println(myString);
// Вивід: This is sentence.
```

## Дивись також

- [Документація Ардуіно для функції `replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace)
- [Документація Ардуіно для функції `remove()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/remove)
- [Чому використовувати рядки в програмуванні на Ардуіно](https://www.makeuseof.com/tag/arduino-strings-usage-tutorial/)