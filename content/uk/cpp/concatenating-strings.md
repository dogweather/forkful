---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?

Стрічки можна об'єднувати, вставляючи одну стрічку в кінець іншого. Ми робимо це, щоб створити нову стрічку, яка містить обидві оригінальні стрічки.

## Як це зробити:

Об'єднання стрічок у C++ можна реалізувати за допомогою оператора '+'. Подивимось на приклад:

```C++
#include<iostream>
#include<string>

int main(){
    std::string str1 = "Привіт, ";
    std::string str2 = "Світе!";
    std::string str3 = str1 + str2; 
    
    std::cout << str3;
    return 0;
}
```
Вихід:
```C++
Привіт, Світе!
```
Зауважте, що ви можете використовувати оператор '+', щоб прямо додати дві стрічки, як це показано у коді.

## Занурення в контекст

Подібно до багатьох мов програмування, C++ дозволяє об'єднувати стрічки разом. Проте, варто відзначити, що об'єднання великої кількості стрічок може бути недостатньо ефективним, оскільки кожен виклик оператора '+' створює новий об'єкт стрічки.

Альтернативою може бути використання `std::ostringstream` або `std::stringstream` для об'єднання стрічок, особливо коли є багато об'єктів для об'єднання.

Ось приклад:
```C++
#include<iostream>
#include<sstream>

int main(){
    std::ostringstream os;
    os << "Привіт, " << "Світе!";
    std::string str3 = os.str();
    
    std::cout << str3 << std::endl;
    return 0;
}
```
Цей метод може бути більш ефективним, ніж просто використання '+', залежно від ситуації.

## Дивіться також

Поглиблене розуміння теми об'єднання стрічок в C++ ви можете знайти за посиланнями:
- ["Basic String Operations"](http://www.cplusplus.com/reference/string/string/) від cplusplus.com
- ["How to concatenate strings in C++"](https://www.fluentcpp.com/2018/12/06/strcat-vs-strlcat/) від fluentcpp.com
- ["Concatenating Strings in C++"](https://www.learncpp.com/cpp-tutorial/concatenating-strings/) від learncpp.com.