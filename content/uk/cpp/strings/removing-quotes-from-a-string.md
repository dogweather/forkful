---
title:                "Видалення лапок зі строки"
aliases:
- /uk/cpp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:26.142757-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?
Видалення лапок із рядка означає усунення тих набридливих подвійних або одинарних символів, які оточують наш текст (' або "). Програмісти часто роблять це, щоб санітувати вхідні дані, зберігати текст у базі даних або підготувати рядки для подальшої обробки без нагромадження лапок.

## Як це зробити:
Ось простий спосіб позбутися лапок у C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Запустіть це, і ви отримаєте:

```
Hello, World!
```

Вуаля! Лапки зникли.

## Поглиблений Огляд
Лапки є набридливістю тексту з самого початку обчислень. Були часи, коли програмісти кропітко пробігалися через кожен символ, щоб відфільтрувати ці лапки. Нині ми маємо `std::remove` у Стандартній Темплейт Бібліотеці (STL) для виконання важкої роботи.

Альтернативи? Звісно! Ви могли б використати регулярні вирази з `std::regex` для націлювання на лапки, але це трохи як використовувати молот для розбивання горішків - потужно, але може бути надміру для простих завдань. Для тих, хто віддає перевагу новітнім версіям C++, можна спробувати `std::string_view` для підходів, що не передбачають модифікації.

Що стосується реалізації, пам'ятайте, що `std::remove` насправді не видаляє елементи з контейнера; він переміщує елементи, що не були видалені, вперед і повертає ітератор за новий кінець діапазону. Тому нам потрібен метод `erase`, щоб відтяти непотрібний хвіст.

## Дивіться Також
- Довідник C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Більше про маніпуляції з `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
