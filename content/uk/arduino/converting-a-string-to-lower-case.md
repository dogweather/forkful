---
title:                "Перетворення рядка на малі букви"
html_title:           "Arduino: Перетворення рядка на малі букви"
simple_title:         "Перетворення рядка на малі букви"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Зменшення регістру рядка (або перетворення рядка в нижній регістр) - це процес, за допомогою якого всі літери у рядку перетворюються в малі літери. Це корисна операція для програмістів, оскільки допомагає зробити рядок однорідним і спрощує порівняння та пошук у рядках.

## Як це зробити:

```Arduino
String name = "Arduino";
name.toLowerCase(); // результат: "arduino"
```

```Arduino
String message = "Привіт, Світ!";
message.toLowerCase(); // результат: "привіт, світ!"
```

## Глибоке занурення:

Практика зменшення регістру походить з традиції зменшення регістру імен на деяких старих мов програмування, таких як Fortran і BASIC. Альтернативою до цього може бути використання методу ```tolower``` з бібліотеки ```<ctype.h>```.

У Arduino, метод ```toLowerCase()``` доступний для строкових об'єктів класу ```String```. Він не приймає жодних аргументів і повертає новий рядок зі зменшеним регістром.

## Дивіться також:

Детальніше про метод ```toLowerCase()``` можна прочитати в офіційній документації Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/tolowercase/ 

Існує також метод ```toUpperCase()```, який перетворює рядок в верхній регістр. Його використання аналогічне методу ```toLowerCase()```.