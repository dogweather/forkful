---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це та навішо?
Пошук та заміна тексту - це процес виявлення окремих фрагментів коду та змінення їх на задані значення. Програмісти роблять це, щоб модифікувати значення змінних, виправити помилки або оновити частини коду.

## Як це зробити:
```C++
#include <iostream>
#include<string>
#include<algorithm>

void replaceSubstring(std::string &s, const std::string &search, const std::string &replace) {
    for(size_t pos = 0; (pos = s.find(search, pos)) != std::string::npos; pos += replace.length()){
        s.replace(pos, search.length(), replace);
    }
}

int main() {
    std::string s = "Hello, World!";
    std::cout << "Before replace: "<< s << std::endl;
    replaceSubstring(s, "World", "Ukraine");
    std::cout << "After replace: "<< s << std::endl;
    return 0;
}
```
**Вихід:**
```
Before replace: Hello, World!
After replace: Hello, Ukraine!
```

## Детальніше
Історично, пошук та заміна тексту були необхідною частиною редагування текстового коду. З появою ООП, концепція простої зміни значень змінних перетворилася в більш складну ідей з заміни тексту. 

Є кілька альтернатив методу пошуку та заміни тексту в C++. Наприклад, ви можете використати метод `std::regex_replace` з бібліотеки `regex`.

Згаданий у прикладі метод витягує субстроку тексту, знаходить її вхождення та замінює на нову. Це процес проходить ітеративно, поки усе знаходження не буде замінено.

## Дивись також
1. [std::regex_replace](http://www.cplusplus.com/reference/regex/regex_replace/)
2. [std::string::replace](http://www.cplusplus.com/reference/string/string/replace/)
3. [std::string::find](http://www.cplusplus.com/reference/string/string/find/)