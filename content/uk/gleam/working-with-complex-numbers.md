---
title:                "Робота з комплексними числами"
date:                  2024-01-26T04:41:00.279039-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що та Чому?
Комплексні числа мають дійсну та уявну частини (`a + bi`). Вони стануть у пригоді в різних галузях, таких як електротехніка та квантові обчислення. Програмісти використовують їх для моделювання рівнянь, які не можуть бути розв'язані, використовуючи лише дійсні числа.

## Як це зробити:
У Gleam немає вбудованої підтримки комплексних чисел. Зазвичай ви створюєте свої реалізації або шукаєте бібліотеку. Ось швидкий приклад того, як ви могли б реалізувати базові операції:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Поглиблений огляд

Комплексні числа вперше були більш формально задокументовані Героламо Кардано у 16 столітті. Вони є природним розширенням дійсних чисел. Однак, в такій молодій мові як Gleam, яка надає пріоритет продуктивності та типовій безпеці, такі можливості є дуже обмеженими (або ви робите DIY).

У деяких інших мовах, наприклад в Python, комплексні числа вбудовані (`3+4j`), що полегшує життя. У Rust або Haskell існують бібліотеки, які з самого початку пропонують розширені функціональні можливості.

Підхід Gleam означає, що ви маєте взяти на себе обробку всіх аспектів: арифметику, полярні координати, експоненціальні форми тощо. Реалізація ефективних, точних операцій вимагає уважного програмування, враховуючи те, як поведінка чисел з плаваючою комою може впливати на ваші результати.

Пам'ятайте ретельно тестувати, особливо крайні випадки! Обробка комплексної нескінченності та значень NaN (не число) може спіткати вас, якщо ви не будете обережні.

## Дивіться також
Для більш цікавих матеріалів, ось де ви можете поглибити знання:
- [Офіційна документація Gleam](https://gleam.run/documentation/)
- Пошукайте натхнення в бібліотеках інших мов, наприклад, бібліотеці [num-complex] Rust(https://crates.io/crates/num-complex) або модулі [cmath] Python(https://docs.python.org/3/library/cmath.html).