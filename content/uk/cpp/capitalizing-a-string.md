---
title:                "Переведення рядка в верхній регістр"
html_title:           "C++: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Заглушання рядків - це процес перетворення всіх букв рядка на великі. Програмісти роблять це для уніфікації тексту та спрощення подальшого порівняння рядків.

## Ось як:

У C++ можна використовувати стандартну бібліотеку `<algorithm>` та `<cctype>` для того, щоб заглушити рядок. Ось швидкий приклад:

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    std::cout << str << std::endl;
    return 0;
}
```

Коли ви запустите цей код, ви побачите наступний вивід:
```
HELLO, WORLD!
```

## Занурення у деталі

Заглушання рядка було відомо десятиліттями та широко використовується у багатьох мовах програмування. І хоча на перший погляд це може здатися тривіальним завданням, є декілька нюансів, які слід взяти до уваги.

Метод `::toupper`, який використовується вище, працює зі знаками ASCII і не розрахований на роботу з не-ASCII символами. Це може бути проблемою, якщо ви працюєте з міжнародними рядками. Для таких ситуацій краще використовувати більш універсальні бібліотеки, такі як ICU.

У C++ є кілька альтернатив заглушанню рядка, таких як `std::toupper` та `boost::algorithm::to_upper_copy`. Вибір між ними залежить від вашого конкретного випадку використання.

## Це може вас зацікавити

1. [Документація C++ по `::toupper`](https://en.cppreference.com/w/cpp/string/byte/toupper)
2. [Документація Boost `to_upper_copy`](https://www.boost.org/doc/libs/1_75_0/doc/html/boost/algorithm/to_upper_copy.html)
3. [ICU - Бібліотека для роботи з міжнародними рядками](http://userguide.icu-project.org/strings)
4. [Що таке ASCII?](https://uk.wikipedia.org/wiki/ASCII)