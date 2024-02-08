---
title:                "Читання аргументів командного рядка"
aliases:
- uk/cpp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:33.660533-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і Чому?
Читання аргументів командного рядка — це отримання вхідних даних для програми напряму з консолі. Програмісти використовують цей прийом для гнучкості: ви задаєте параметри запуску та налаштовуєте програму без зміни коду.

## Як це зробити:
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "You have entered " << argc << " arguments:" << std::endl;
    
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }
    
    return 0;
}
```
При запуску програми `./program Hello World!`, виведе:
```
You have entered 3 arguments:
0: ./program
1: Hello
2: World!
```

## Поглиблений аналіз
Традиційно, `argc` вказує кількість аргументів, а `argv` — це масив рядків, кожен з яких є одним аргументом. Це старовинний спосіб з часів мови С, проте і в сучасному C++ використовується часто. Як альтернативу, можна використати бібліотеки, такі як Boost.Program_options чи TCLAP, які дозволяють зручніше парсити командний рядок. Розширені можливості включають валідацію введення, автоматичне створення документації щодо параметрів тощо. Власне реалізація читання командного рядка залежить від операційної системи.

## Дивіться також
- Документація по бібліотеці Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- TCLAP - Templated C++ Command Line Parser Library: http://tclap.sourceforge.net/
- GNU getopt library for parsing command-line options: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
