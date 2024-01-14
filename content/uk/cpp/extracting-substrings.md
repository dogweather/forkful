---
title:                "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому

Виділення підрядків є важливим елементом програмування в С++, яке дозволяє нам вибирати та обробляти конкретні частини тексту або символів. Це може бути корисним, коли ми хочемо взаємодіяти з користувачем або здійснювати пошук та заміну певних даних.

## Як

Для отримання підрядків у С++, нам потрібно використовувати функції, які надаються стандартною бібліотекою, такі як `substr()` та `find()`. Приклад коду наведено нижче, де ми вибираємо підрядок "world" із рядка "Hello world!":

```C++
#include <iostream>
using namespace std;

int main() {
    string str = "Hello world!";
    string sub_str = str.substr(6, 5);
    cout << sub_str << endl;
    return 0;
}
```

Вивід: `world`

У цьому прикладі ми використовуємо функцію `substr()` для вибору підрядка "world" за допомогою індексів 6 та 5, що вказують на початок та довжину підрядка. Також, для отримання позиції підрядка у вихідному рядку, ми можемо використовувати функцію `find()`, яка повертає індекс першого знайденого входження підрядка. Наприклад:

```C++
#include <iostream>
using namespace std;

int main() {
    string str = "Hello world!";
    int pos = str.find("world");
    cout << pos << endl;
    return 0;
}
```

Вивід: `6`

## Глибока занурення

Крім стандартних функцій, С++ також дозволяє використовувати регулярні вирази для виділення підрядків. Регулярні вирази дозволяють нам здійснювати більш гнучкий та точний пошук та заміну підрядків. Для роботи з регулярними виразами у С++, нам потрібно підключити бібліотеку `<regex>` та використовувати функцію `regex_search()`. Приклад коду наведено нижче, де ми використовуємо регулярний вираз для знаходження всіх слів в рядку:

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string str = "Hello world!";
    regex pattern("\\b(\\w+)\\b");
    smatch matches;
    while (regex_search(str, matches, pattern)) {
        cout << matches[0] << endl;
        str = matches.suffix().str();
    }
    return 0;
}
```

Вивід: `Hello` `world`

### Додаткові ресурси

- [Документація з роботи з підрядками у С++](https://www.cplusplus.com/reference/string/string/find/)
- [Документація з роботи з регулярними виразами у С++](https://www.cplusplus.com