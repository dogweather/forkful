---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що та чому?

Отримання поточної дати в C++ - це процес виведення часу, що ідеться просто зараз. Програмісти роблять це для відстеження часу виконання, журналів, таймстампів та інших задач.

## Як здійснити:

Це приклад коду для отримання поточної дати в C++.

```C++
#include<iostream>
#include<ctime>

int main() {
    time_t now = time(0);
    char* theDate = ctime(&now);
    std::cout << "The current date and time is: " << theDate << std::endl;
    return 0;
}
```

Якщо ви скомпілюєте і виконаєте це, отримаєте вивід, подібний до наведеного нижче:

```C++
The current date and time is: Wed Apr 7 14:42:10 2021
```

## Занурення в тему:

Пам'ятайте, що в C++ існує декілька способів отримання поточної дати чи часу. Бібліотека `<ctime>` є вбудованою в C++, але є ще новіту бібліотеку `chrono` в C++11. Семантика останньої важча для розуміння, але вона надає більше можливостей і точність.

```C++
#include<iostream>
#include<chrono>
#include<ctime>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t theDate = std::chrono::system_clock::to_time_t(now);
    std::cout << "The current date and time is: " << std::ctime(&theDate) << std::endl;
    return 0;
}
```

## Див. також:

1. Туторіал Cplusplus по роботі з часом: http://www.cplusplus.com/reference/ctime/
2. Офіційна документація C++11 chrono: http://www.cplusplus.com/reference/chrono/