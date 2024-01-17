---
title:                "Отримання підрядка"
html_title:           "C: Отримання підрядка"
simple_title:         "Отримання підрядка"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/extracting-substrings.md"
---

{{< edit_this_page >}}

Що & Чому?
Екстракція підрядків - це процес витягування певної частини тексту з рядка в програмі для подальшого використання. Програмісти часто використовують цей метод для отримання необхідної інформації з великих рядків або для обробки даних у певному форматі.

Як це зробити:
```C
char str[20] = "programming is fun";
char substr[10];
int index = 5;
int length = 2;
//extracting substring "in"
strncpy(substr, str + index, length);
printf("%s", substr);
```
Вивід: "in"

```C
char str[] = "Hello World!";
char substr1[6];
char substr2[6];
int index = 6;
//extracting substrings "Hello" and "World"
strncpy(substr1, str, index);
strncpy(substr2, str + index, strlen(str) - index);
printf("%s, %s!", substr1, substr2);
```
Вивід: "Hello, World!"

Глибше розбираємось:
Екстракція підрядків була популярна задовго до сучасного програмування. Наприклад, в редакторі тексту Vi, програміст міг обрізати або копіювати певну частину тексту з рядка. У деяких мовах програмування, таких як Python, є спеціальні функції для екстракції підрядків. У C це зробити дещо складніше, але все ж можливо за допомогою функцій, таких як ```strncpy()``` та ```strlen()```.

Дивіться також:
- [Стаття про функцію strncpy в C](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Порівняння різних способів екстракції підрядків у C](https://www.geeksforgeeks.org/efficiently-extract-a-substring-from-a-string-in-c/)
- [Приклади роботи з рядками у C](https://www.programiz.com/c-programming/c-strings)