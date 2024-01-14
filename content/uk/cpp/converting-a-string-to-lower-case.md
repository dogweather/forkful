---
title:                "C++: Перетворення рядка в нижній регістр"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому
Перетворення рядка в нижній регістр - це важлива операція в програмуванні, оскільки допомагає зробити дані більш уніфікованими і полегшує подальшу обробку і порівняння. Цей процес також може використовуватися для полегшення пошуку та фільтрації даних.

## Як
```C++
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

int main() {
    string input;
    cout << "Введіть рядок: ";
    getline(cin, input); // отримуємо ввід від користувача

    transform(input.begin(), input.end(), input.begin(), ::tolower); // застосовуємо функцію перетворення в нижній регістр

    cout << "Перетворений рядок: " << input << endl;

    return 0;
}
```

**Ввід:**
```
Hello World
```
**Виведення:**
```
hello world
```

## Глибока Занурення
Перетворення рядка в нижній регістр - це процес перетворення заголовків букв в рядку (включаючи великі літери) в їх еквіваленти в нижньому регістрі. Цей процес застосовується до кожного символу в рядку за допомогою функції `transform()` з бібліотеки `<algorithm>`. Одна з важливих особливостей цього процесу - позбавленняся відчуття різниці між великими і малими літерами, що дозволяє легше порівнювати рядки.

## Дивись також
- [transform() функція в C++](https://www.geeksforgeeks.org/transform-function-cpp/)
- [Операції з рядками в C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)