---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:55:44.296246-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Читання аргументів командного рядка дає програмам гнучкість. Ви використовуєте це, щоб передавати дані в програму при її запуску, встановлювати опції або напряму комунікувати з виконавчою логікою.

## How to (Як це зробити):
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("You have entered %d command-line argument(s):\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("%s\n", argv[i]);
    }
    return 0;
}
```
Запустіть так: `./your_program arg1 arg2`.
Вивід буде:
```
You have entered 2 command-line argument(s):
arg1
arg2
```

## Deep Dive (Поглиблений Розбір):
Командні аргументи — стара ідея. З ранніх днів UNIX, вони дозволяли взаємодію з програмами через shell. `argc` (argument count) — це кількість аргументів, а `argv` (argument vector) — масив рядків, що містить самі аргументи. В `argv[0]` зазвичай ім'я програми, тому ваш код читає аргументи, починаючи з `argv[1]`.

Альтернативи? Модулі як `getopt()` чи бібліотеки, наприклад, Argp або Boost.Program_options в С++ для складніших завдань.

Реалізація? Шелл або операційна система передає аргументи в вашу програму. C стандарт не визначає, як це робити, тому відповідальність лежить на компіляторі або системі.

## See Also (Дивись також):
- [GNU C Library Manual - Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [C Standard - ISO/IEC 9899:2018](https://www.iso.org/standard/74528.html)
- [POSIX standard for command line arguments](https://pubs.opengroup.org/onlinepubs/9699919799/)
