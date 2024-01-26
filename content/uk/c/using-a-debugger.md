---
title:                "Використання дебагера"
date:                  2024-01-26T03:48:07.543412-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання дебагера"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Що і чому?
Дебагер — це інструмент, який дозволяє вам оглядати ваш код на C під час його виконання, крок за кроком, щоб відстежити помилки. Програмісти користуються дебагерами, щоб розуміти, як поводиться їхній код, виправляти проблеми і оптимізувати продуктивність, не граючи в вгадування.

## Як це зробити:
Скажімо, ви працюєте з простою C програмою, яка обчислює факторіал числа, але є збій. Щоб користуватися дебагером, як-от `gdb` (GNU Debugger), спершу скомпілюйте з прапором `-g`, щоб включити інформацію для дебагінгу:

```c
// компілювати з: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Проста перевірка на негативне введення
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Факторіал числа %d дорівнює %ld\n", number, result);
    return 0;
}
```

Потім запустіть його в gdb:

```shell
$ gdb ./factorial
```

Встановіть точку зупинки на функції `factorial` і запустіть програму:

```gdb
(gdb) break factorial
(gdb) run
```

Коли виконання дійде до точки зупинки, пройдіться крізь кожен рядок, використовуючи `next` або `n`, і переглядайте змінні за допомогою `print` або `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Зразковий вивід надасть реальні значення та потік виконання програми.

## Заглиблення
Дебагери існують з 1960-х років, еволюціонувавши від простих моніторів до складних додатків з графічним інтерфейсом користувача. Друкований дебагінг був поширений до винайдення зрілих дебагерів. Альтернативи `gdb` включають `lldb`, `dbx` або дебагери, інтегровані в IDE, як от у Visual Studio або CLion.

При роботі з дебагерами реалізація варіюється — деякі можуть зловити помилки виконання, оглянути пам'ять або навіть змінити напрямок виконання програми. `gdb` може приєднуватися до уже запущених процесів, дозволяючи дебагінг вже працюючого програмного забезпечення, що є благом для виправлення помилок на живих системах.

## Дивіться також
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Техніки дебагінгу в C: http://www.cprogramming.com/debugging/debugging.html