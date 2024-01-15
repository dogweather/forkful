---
title:                "Видобування підрядків"
html_title:           "Java: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому
Нерідко у програмуванні доводиться зустрічатися зі словами чи символами, які потрібно виділити зі строки. Наприклад, отримати частину електронної адреси до символу "@" або видалити зайві пробіли з початку і кінця рядка. Для цього існує така корисна функція, як витягування підрядка. 

## Як це зробити
Для витягування підрядка в Java існує метод `substring()`, який можна викликати на змінній, яка містить вихідний рядок. Для позначення початку та кінця підрядка використовується індексація, де перший символ має індекс 0. Наприклад:
```Java
String email = "example@email.com";
// Вибираємо підрядок до символу "@"
String username = email.substring(0, email.indexOf("@"));
System.out.println(username); // виведе "example"

// Видалення зайвих пробілів на початку і кінці рядка
String s = "  hello world  ";
String trimmed = s.substring(s.trim(), s.length() - s.trim());
System.out.print(trimmed); // виведе "hello world"
```

## Глибоке дослідження
Поміж індексацією символів у Java є ще така концепція, як операції зі зсувом. Наприклад, для отримання символів у рядку ми можемо використовувати методи, які вимагають індекс першого символу і кількість символів, які потрібно витягнути. Але можна також використовувати легше для зрозуміння і менш підприємницьке вираження `start + length`, де `start` - це індекс початку підрядка, а `length` - кількість символів, які потрібно витягнути. Наприклад:
```Java
// Використання індексації
String s = "hello world!";
String sub = s.substring(6, 11); // витягне "world"

// Використання зсуву
String s = "hello world!";
int start = 6; // індекс початку підрядка
int length = 5; // кількість символів для витягнення
String sub = s.substring(start, start + length); // витягне "world"
```

## See Also
Для детальнішого вивчення можна прочитати документацію по класу `String` та методу `substring()`, а також переглянути приклади на сайті Stack Overflow:
- Документація по класу `String`: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Документація по методу `substring()`: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int)
- Приклади використання на Stack Overflow: https://stackoverflow.com/search?q=java+substring