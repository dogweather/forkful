---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що І Чому?

Інтерфейс командного рядка (Command Line Interface, CLI) дозволяє користувачам interagit' з програмою за допомогою команд уведених через рядок введення. Для програмістів, читання аргументів командного рядка дає змогу керувати поведінкою програми за умовчанням або налаштувати її під кастомні потреби.

## Як до:

Щоб прочитати аргументи з командного рядка, використовуйте основний метод `main()`. Приклад:

```C++
#include <iostream>

int main(int argc, char** argv) {
    for(int i = 1; i < argc; i++) {
        std::cout << "Аргумент " << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

У цьому прикладі, `argc` представляє кількість аргументів командного рядка, а `argv` -- це масив цих аргументів. Спробуйте запустити такий код з аргументами, ви отримаєте наступний вивід:

```C++
Аргумент 1: тест
Аргумент 2: 123
```

## Поглиблення:

Читання аргументів командного рядка є дуже старим концептом, що иснує ще з часів Unix'овських систем. Але чи існують альтернативи? Звісно, такі як використання файлів конфігурації, environment'ових змінних, налаштування через UI або API, але командний рядок все ще використовується через свою простоту і універсальність.

## Дивіться також:

1. ["Command line arguments in C/C++"](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
2. ["Command Line Interfaces"](https://tldp.org/LDP/intro-linux/html/sect_03_02.html)

Remember, be creative, and use these capabilities to build more dynamic, flexible programs.