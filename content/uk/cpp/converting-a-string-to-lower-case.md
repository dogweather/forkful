---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Перетворення рядків у нижній регістр - це процес, коли всі великі букви у рядку перетворюються на малі. Це корисно для унормовування даних і спрощення порівняння рядків.

## Як це робиться:

Для перетворення рядків у нижній регістр у C++, ви можете використовувати стандартну функцію 'tolower' із бібліотеки 'cctype'.

```C++
#include <iostream>
#include <cctype>
#include <algorithm>

int main() {
    std::string str = "Hello, World!";
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    std::cout << str;
    return 0;
}
```

Виходом цього коду буде `"hello, world!"`.

## Поглиблено:

Перетворення рядків у нижній регістр - це відносно стара техніка, яка використовується разом зі стародавніми системами обробки тексту. Зберігання і зіставлення рядків у нижньому регістрі можуть спростити процес знаходження співпадінь.

Альтернативи можуть включати використання регулярних виразів або можливостей Об'єктно-Орієнтованого Програмування для створення власних функцій.

C++ `tolower` працює, перетворюючи юнікод-символи в їх еквіваленти нижнього регістру. Проте, вона працює тільки на окремі символи, тому для рядків ми використовуємо алгоритм `std::transform`.

## Дивись також:

- [C++ Reference: `std::transform`](http://www.cplusplus.com/reference/algorithm/transform/)
- [C++ Reference: `std::tolower`](http://www.cplusplus.com/reference/cctype/tolower/)
- [StackOverflow: how to implement tolower in C++](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)