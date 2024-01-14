---
title:                "C++: Пошук довжини рядка"
simple_title:         "Пошук довжини рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому 

Знаходження довжини рядка - корисна вміння при програмуванні на C++. Вона дозволяє отримати інформацію про кількість символів, що містяться в рядку, що може бути корисно при обробці текстових даних.

## Як 

```C++
#include <iostream> 
using namespace std; 
  
int main() 
{ 
    // Визначаємо рядок 
    string str = "Привіт, Світ!"; 
  
    // Знаходимо довжину рядка за допомогою функції length() 
    int len = str.length(); 
  
    // Виводимо результат 
    cout << "Довжина рядка '" << str << "' - " << len << " символів"; 
  
    return 0; 
} 
```
Вивід:
Довжина рядка 'Привіт, Світ!' - 13 символів

## Огляд 

За допомогою функції length() ми можемо отримати кількість символів у рядку, що дозволяє нам уникнути використання циклу для перебору всіх символів. Також існує функція size(), яка виконує те ж саме завдання. Ці функції повертають значення типу int, тому що кількість символів може бути доступна у вигляді цілого числа.

## Дивись також 

- [C++ Strings](https://www.programiz.com/cpp-programming/strings)
- [Рядки у C++](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String)