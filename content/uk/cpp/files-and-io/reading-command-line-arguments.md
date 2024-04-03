---
date: 2024-01-20 17:55:33.660533-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.872646-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
weight: 23
---

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
