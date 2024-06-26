---
date: 2024-01-26 01:10:14.943081-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u0412\u0456\u0437\
  \u044C\u043C\u0435\u043C\u043E \u0437\u0430\u0433\u0430\u043B\u044C\u043D\u0435\
  \ \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F: \u0440\u043E\u0437\u0440\u0430\
  \u0445\u0443\u043D\u043E\u043A \u043F\u043B\u043E\u0449\u0456 \u043A\u0440\u0443\
  \u0433\u0430. \u0417\u0430\u043C\u0456\u0441\u0442\u044C \u0442\u043E\u0433\u043E\
  , \u0449\u043E\u0431 \u043F\u0438\u0441\u0430\u0442\u0438 \u0442\u0443 \u0441\u0430\
  \u043C\u0443 \u0444\u043E\u0440\u043C\u0443\u043B\u0443 \u043A\u043E\u0436\u043D\
  \u043E\u0433\u043E \u0440\u0430\u0437\u0443, \u043C\u0438 \u0437\u0430\u043A\u043B\
  \u044E\u0447\u0430\u0454\u043C\u043E \u0457\u0457 \u0432 \u0444\u0443\u043D\u043A\
  \u0446\u0456\u044E."
lastmod: '2024-03-13T22:44:49.854250-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0456\u0437\u044C\u043C\u0435\u043C\u043E \u0437\u0430\u0433\u0430\
  \u043B\u044C\u043D\u0435 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F."
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0432 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
weight: 18
---

## Як робити:
Візьмемо загальне завдання: розрахунок площі круга. Замість того, щоб писати ту саму формулу кожного разу, ми заключаємо її в функцію.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Площа круга з радіусом " << r << " є " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Приклад виведення:
```
Площа круга з радіусом 5 є 78.5397
```

## Поглиблено
Історично процедури та функції були основою структурованого програмування, яке було просунуте в 1960-х роках для боротьби з проблемою "спагетті-коду" в ранніх імперативних програмних мовах. Альтернативи, як ООП (Об'єктно-орієнтоване програмування), заходять ще далі, асоціюючи ці функції зі структурами даних. У C++ у вас є звичайні функції, методи класів (включаючи статичні методи), лямбди та шаблонні функції, кожна з яких пропонує різні переваги. Реалізація добре організованих функцій зазвичай передбачає дотримання принципів, як DRY ("Не повторюйся") і SRP (Принцип єдиної відповідальності), що означає, що кожна функція робить лише одне і робить це добре.

## Див. також
Для більш детальної інформації про функції в C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Про принципи проектування, пов'язані з функціями:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Дізнайтеся про лямбди та розширене використання функцій:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
