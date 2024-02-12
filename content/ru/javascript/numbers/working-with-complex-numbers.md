---
title:                "Работа с комплексными числами"
aliases: - /ru/javascript/working-with-complex-numbers.md
date:                  2024-01-29T00:05:20.281079-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с комплексными числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/working-with-complex-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Комплексные числа — это числа с действительной и мнимой частями (например, 3 + 4i). Они возникают в различных задачах программирования, особенно при обработке сигналов, в квантовых вычислениях и при решении уравнений с полиномами. Программисты манипулируют ими для эффективного решения таких задач.

## Как:
В JavaScript нет встроенной поддержки комплексных чисел, но вы можете смело закатать рукава и управлять ими с помощью объектов и математики. Вот краткий обзор.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...добавьте больше методов (вычитание, умножение, деление) по мере необходимости

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`Результат: ${result}`); // Результат: 4 + 6i
```

## Глубокое погружение
Комплексные числа существуют с 16 века, благодаря итальянскому математику Джероламо Кардано. Они стали критически важны в различных областях, как инженерия и физика. В современном программировании они ключевые для симуляций и алгоритмов, требующих многомерности.

Теперь, JavaScript изначально не предназначен для работы с комплексными числами. Но, помимо DIY-варианта, можно использовать математические библиотеки, такие как math.js или numeric.js. Они обладают мощностями для более серьезной работы с комплексными числами, добавляя такие преимущества, как больше операций, вычисление величины и нахождение аргумента.

Под капотом, когда вы работаете с комплексными числами, это как управление двумя отдельными числами, связанными на бедрах. Сложение и вычитание — это легкие действия — соответствуете действительное с действительным, мнимое с мнимым. Умножение и деление становятся пикантнее с перекрестными терминами и требуют большей осторожности.

## Смотрите также
- Веб-документация MDN по JavaScript: https://developer.mozilla.org/ru/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, математическая библиотека, включающая комплексные числа: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, еще одна библиотека: http://numericjs.com/documentation.html
- Более глубокое погружение в комплексные числа (с акцентом на математику): https://mathworld.wolfram.com/ComplexNumber.html
