---
title:                "Пошук і заміна тексту"
html_title:           "C++: Пошук і заміна тексту"
simple_title:         "Пошук і заміна тексту"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Шукаєте і замінюєте текст в C++ допомагає здійснити зміни великою кількості даних швидко та ефективно. Це особливо корисно при роботі з великими проектами.

## Як

Існує кілька способів шукати та замінювати текст у C++. Один з них дозволяє використовувати функцію `std::string::replace` для заміни всіх входжень одного слова на інше.

```
#include <iostream>
#include <string>
using namespace std;

int main() {
  string s = "Привіт, світ!";
  s.replace(s.find("світ"), 4, "планета"); // замінюємо слово "світ" на "планета"
  cout << s << endl; // виводить "Привіт, планета!"
}
```

Іншим способом є використання бібліотеки `boost::regex` для знаходження та заміни шаблонів у тексті.

```
#include <iostream>
#include <boost/regex.hpp>
using namespace std;

int main() {
  string s = "Цей рядок jsut rain me";
  boost::regex reg("jsut"); // створюємо шаблон пошуку
  string result = boost::regex_replace(s, reg, "just"); // замінюємо "jsut" на "just"
  cout << result << endl; // виводить "Цей рядок just rain me"
}
```

## Deep Dive

При використанні функції `std::string::replace`, потрібно вказати позицію початку замінюваного слова та його довжину. Це можна зробити за допомогою функцій `find` та `length`. Також можна використовувати ітератори для уточнення позиції початку слова.

Бібліотека `boost::regex` також має багато додаткових функцій, які дозволяють виконувати більш складні заміни, такі як заміна з урахуванням регістру, шаблонів з регулярними виразами, тощо.

## See Also

- [Документація C++ для функції `std::string::replace`](https://www.cplusplus.com/reference/string/string/replace/)
- [Офіційна документація C++ для бібліотеки `boost::regex`](https://www.boost.org/doc/libs/1_76_0/libs/regex/doc/html/index.html)