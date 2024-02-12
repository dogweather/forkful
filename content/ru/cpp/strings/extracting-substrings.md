---
title:                "Извлечение подстрок"
aliases:
- /ru/cpp/extracting-substrings.md
date:                  2024-01-28T23:57:28.166201-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Извлечение подстрок означает выделение маленьких кусочков из большей строки. Программисты делают это, чтобы изолировать, обработать или анализировать конкретные данные в тексте, например, извлекать имена пользователей из адресов электронной почты или даты из журналов.

## Как это делать:

C++ облегчает задачу извлечения подстрок. `std::string` здесь наш надежный помощник, с функцией `substr()`, которая выполняет большую часть работы. Давайте перейдем непосредственно к коду:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World! Programming in C++ is fun.";
    std::string snippet;

    // Извлекаем "World", начиная с индекса 7 и длиной 5
    snippet = fullString.substr(7, 5);
    std::cout << snippet << std::endl; // Вывод: World

    // Извлекаем "Programming", начиная с индекса 14
    snippet = fullString.substr(14);
    std::cout << snippet << std::endl; // Вывод: Programming in C++ is fun.

    return 0;
}
```

## Подробнее

Подстроки - это не новинка. Программисты на языке С использовали `strncpy` и вели ручной учет. Обработка строк - это распространенный источник ошибок, поэтому C++ стремился это упростить. `std::string` и его метод `substr` существуют с C++98 и с тех пор облегчают жизнь.

Альтернативы? Конечно. Можно пойти более сложным путем с `std::string::iterator` или вернуться к функциям языка C, если вам нравятся риски. Более современный подход может включать string_views для неизменяющего просмотра.

Реализация? Под капотом `substr` часто выделяет новое хранилище и копирует данные, что не бесплатно. Это легче, чем возиться с сырыми указателями и массивами символов добрых старых времен, но это не мгновенно.

## Смотрите также

Для получения дополнительной информации о `std::string` и его друзьях:
- cppreference.com о `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Больше о `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- Обработка строк в стиле C (для исторического интереса): http://www.cplusplus.com/reference/cstring/
