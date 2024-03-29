---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:45.364497-07:00
description: "\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u044B \u043A\u043E\u043C\
  \u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438 \u043F\
  \u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u0435\u043B\u044F\u043C \u0432\u043B\u0438\u044F\u0442\u044C\
  \ \u043D\u0430 \u043F\u043E\u0432\u0435\u0434\u0435\u043D\u0438\u0435 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u044B \u0431\u0435\u0437 \u0438\u0437\u043C\
  \u0435\u043D\u0435\u043D\u0438\u044F \u043A\u043E\u0434\u0430. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044E\u0442 \u0438\u0445 \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\
  \u043D\u0438\u044F \u0432\u0445\u043E\u0434\u043D\u044B\u0445\u2026"
lastmod: '2024-03-13T22:44:45.635572-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u044B \u043A\u043E\u043C\
  \u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438 \u043F\
  \u043E\u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u0442\u0435\u043B\u044F\u043C \u0432\u043B\u0438\u044F\u0442\u044C\
  \ \u043D\u0430 \u043F\u043E\u0432\u0435\u0434\u0435\u043D\u0438\u0435 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u044B \u0431\u0435\u0437 \u0438\u0437\u043C\
  \u0435\u043D\u0435\u043D\u0438\u044F \u043A\u043E\u0434\u0430. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044E\u0442 \u0438\u0445 \u0434\u043B\u044F \u043F\u043E\u043B\u0443\u0447\u0435\
  \u043D\u0438\u044F \u0432\u0445\u043E\u0434\u043D\u044B\u0445\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и почему?
Аргументы командной строки позволяют пользователям влиять на поведение программы без изменения кода. Программы используют их для получения входных параметров, пути к файлу или режима операции, экономя время и предоставляя гибкость.

## Как это сделать:
В C++ аргументы командной строки принимаются в `main()` в виде массива указателей на символы. Вот как вы можете их получить:

```C++
#include <iostream>
int main(int argc, char* argv[]) {
    std::cout << "Вы ввели " << argc << " аргументов:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << argv[i] << "\n";
    }
    return 0;
}
```

Пример вывода: (Предполагая выполнение как `./myProgram foo bar`)

```plaintext
Вы ввели 3 аргумента:
./myProgram
foo
bar
```

## Подробнее
Давным-давно командная строка была единственным способом взаимодействия с программами. Современные графические интерфейсы великолепны, но командная строка сохраняется, особенно в серверных или разработческих средах. Она предлагает быстрое, скриптовое управление.

Альтернативы встроенным `argv` и `argc` включают в себя библиотеки, такие как `Boost.Program_options` для более изысканного разбора. Также есть функция `getopt()` в системах, подобных Unix, для более традиционных поклонников командной строки.

Реализация разбора аргументов с нуля позволяет вам настроить его, но следите за безопасностью. Не доверяйте пользовательскому вводу слепо — всегда проверяйте и очищайте данные.

## См. также
- Документация C++ по функции `main()`: https://en.cppreference.com/w/cpp/language/main_function
- Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- Учебное пособие по `getopt()` от GNU: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
