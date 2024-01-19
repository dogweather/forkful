---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

##  Що & Чому?

Підстроки - це набори символів з основного рядка. Ми використовуємо їх, коли хочемо обробляти частину рядка замість всього рядка для економії ресурсів.

## Як це робимо:

Ось процес витягування підстрок: 

```C++
#include<iostream>
#include<string>
using namespace std;

int main() {
  string str = "Ми любимо програмування";
  string substr = str.substr(3,7);
  cout << substr;
    
  return 0;
}
```
Виведення: 

```C++
любимо
```
Функція substr(start, length) створює підстроку починаючи з позиції "start" і довжиною в "length" символів.

## Більш глибоке занурення

1. Розшифровка SubStr історично вийшла з "substring", що означає "підстрока", і широко використовувалась з початку днів програмування.

2. В альтернативних мовах програмування, подібних до JavaScript, ми можемо використовувати метод slice() для витягування підстрок. З іншого боку, в Python є висічки рядків.

3. Робота substr() в C++ полягає у створенні нового об'єкта string, який включає вказані символи з основного рядка. Ця робота здійснюється шляхом копіювання символів до нового об'єкту.

## Дивіться також

1. Стрічкова висічка в Python: [Python Slice](https://docs.python.org/3/whatsnew/2.3.html#slice)
2. Використання slice() в Javascript: [JavaScript Slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice)
3. Більше про C++ substr(): [cppreference substr](https://en.cppreference.com/w/cpp/string/basic_string/substr)
4. Історія String в комп'ютерних науках: [Wikipedia](https://en.wikipedia.org/wiki/String_(computer_science))