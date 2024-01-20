---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/printing-debug-output.md"
---

{{< edit_this_page >}}

Sure, here is how the C programming article might go:

# ⛓ Debug Printf в C: Як і навіщо?

## Що це і чому?

Debug printf — це метод виведення діагностичного виводу в програмі. Програмісти використовують це, щоб швидко та ефективно знаходити та виправляти помилки у своєму коді.

## Як це робиться:

Ось приклад коду, що використовує debug printf:

```C
#include <stdio.h>

int main() {
    #ifdef DEBUG
    printf("Debug: Почати виконання програми.\n");
    #endif

    // і тут весь ваш код...

    #ifdef DEBUG
    printf("Debug: Завершення виконання програми.\n");
    #endif

    return 0;
}
```

З цього прикладу, якщо ви скомпілюєте код з визначенням DEBUG (`gcc -DDEBUG main.c`), то бачитимете текст debug-повідомлень.

## Поглиблений погляд

- **Історичний контекст**: Функція `printf` була вперше представлена в 1972 році в мові сі, і з тих пір використовується для виведення повідомлень в консолі.

- **Альтернативи**: В деяких випадках програмісти можуть використовувати `fprintf()` для виведення помилок в файл, або `sprintf()` для збереження повідомлень у рядку.

- **Деталі реалізації**: Компанія POSIX визначає додаткові особливості `printf`. Ви можете виводити числа, символи, рядки тощо, вказавши відповідний форматний специфікатор (наприклад, `%d` для чисел).

## Ще почитати

- Функція printf визначена в стандартній бібліотеці Сі. Більше тут [printf](http://www.cplusplus.com/reference/cstdio/printf/)
- Хоча `printf` це класика, в сучасному програмуванні C часто використовуються більш комплексні техніки дебагування, такі як gdb або static analysis. Дізнайтеся більше про gdb [тут](https://sourceware.org/gdb/current/onlinedocs/gdb/) та про static analysis [тут](https://scan.coverity.com/tutorials).