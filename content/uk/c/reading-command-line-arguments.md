---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Читання аргументів командного рядка - це можливість обробляти вхідні параметри, які передаються у вашу програму через командний рядок. Це полегшує тестування та збільшує гнучкість вашого коду.

## Як це зробити:

Ось базовий приклад того, як читати аргументи командного рядка в C:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  int count;

  printf("Лічильник аргументів: %d\n", argc);

  for (count = 0; count < argc; count++) {
    printf("argv[%d] = %s\n", count, argv[count]);
  }

  return 0;
}
```
Якщо ви запустите програму з аргументами, наприклад `./prog a b c`, виходом буде:

```C
Лічильник аргументів: 4
argv[0] = ./prog
argv[1] = a
argv[2] = b
argv[3] = c
```

## Занурення в деталі

Читання вхідних аргументів з командного рядка - надзвичайно важлива ознака мови C, яка була наявна практично з самого її створення. Альтернативою є вживання вводу/виводу файлів, але це може бути слабший варіант, особливо для цілей тестування.

`argc` (argument count) містить число аргументів, переданих програмі, тоді як `argv` (argument vector) - це масив `char` покажчиків, які вказують на рядки, що представляють кожен окремий аргумент.

## Дивись також

1. У [GNU libc documentation](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html) ви знайдете подробиці про цю тему.
2. Погляньте на офіційний [C Standard](http://www.open-std.org/JTC1/SC22/wg14/www/standards.html) для глибшого розуміння мови C.
3. Курс [Learn C](https://www.learn-c.org/) надає детальну інформацію про інші аспекти мови C.