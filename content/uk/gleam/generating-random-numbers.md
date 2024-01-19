---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?
Генерація випадкових чисел – це процес створення чисел, які не залежать одне від одного та є незпровідуємими. Програмісти використовують це для моделювання непередбачуваних подій, тестування програм та гарантованої безпеки даних.

## Як це робити:
У Gleam використовується функція `gen` в модулі `gleam/otp/random`. Ось приклад її використання:

```Gleam
import gleam/otp/random.{gen}
import gleam/int.{@println}

pub fn random_number() {
  let number = random.gen(1, 10)
  int.@println(number)
}
```

Цей код виведе випадкове число в діапазоні від 1 до 10.

## Поглиблений аналіз:
Генерація випадкових чисел була критичною частиною комп'ютерних наук з моменту її зародження. Певні методи, такі як лінійний конгруентний метод були дуже популярні в минулому. 

В альтернативних мовах, таких як Python або JavaScript, ви можете використовувати вбудовані методи, такі як `random()` або `Math.random()` для генерації випадкових чисел. 

У Gleam реалізація генерації випадкових чисел базується на Erlang/OTP, що гарантує хороший рівень випадковості та високу продуктивність.

## Див. також:
Лінки з більш докладною інформацією про тему:
1. [Генерація випадкових чисел в мові программування Python](https://docs.python.org/3/library/random.html) 
2. [Erlang/OTP Random Module](http://erlang.org/doc/man/rand.html)
3. [Gleam Language Official Docs](https://gleam.run/docs/)