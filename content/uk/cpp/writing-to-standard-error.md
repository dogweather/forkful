---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Стандартний потік помилок, або stderr, використовують для виведення повідомлень про помилки та інших критичних повідомлень. Це дозволяє відділити звичайний вивід програми від повідомлень про помилки, що спрощує діагностику та логування.

## Як це робити:
```C++
#include <iostream>

int main() {
    // Звичайний вивід
    std::cout << "Hello, World!" << std::endl;

    // Вивід помилок
    std::cerr << "Error: Something went wrong!" << std::endl;

    return 0;
}
```
Якщо запустити код, отримаємо:
```
Hello, World!
Error: Something went wrong!
```
## Поглиблений огляд:
У UNIX-подібних системах `stderr` має файловий дескриптор 2, відрізняється від `stdout`, котрий має дескриптор 1. Використання `stderr` дозволяє направляти помилки в інше місце, ніж звичайний вивід, іноді в файл за допомогою перенаправлення у shell. Альтернативою `cerr` є запис у лог-файли через спеціалізовані бібліотеки чи фреймворки для логування. Пряма робота з `stderr` забезпечує мінімальну затримку, оскільки вона зазвичай не буферизується.

## Також дивіться:
- [cplusplus.com: iostream library](https://www.cplusplus.com/reference/iostream/)
- [cppreference.com: Error handling](https://en.cppreference.com/w/cpp/error)
- [GNU.org: Standard Streams](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
