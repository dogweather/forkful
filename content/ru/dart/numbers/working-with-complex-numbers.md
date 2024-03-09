---
title:                "Работа с комплексными числами"
date:                  2024-03-08T21:58:06.026585-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Комплексные числа, состоящие из действительной и мнимой части (обычно обозначаемые как a + bi), расширяют понятие безразмерных чисел до двумерного пространства. Программисты работают с комплексными числами в таких областях, как электротехника, квантовые вычисления и динамика жидкости, чтобы моделировать явления, которые не могут быть представлены в одном измерении действительных чисел.

## Как это сделать:

В Dart нет встроенной библиотеки для работы с комплексными числами, что требует либо создания пользовательского класса комплексных чисел, либо использования сторонней библиотеки. Популярным выбором для научных вычислений, включая поддержку комплексных чисел, является `package:scidart`.

### Реализация базового класса комплексного числа

Для простых операций вы можете легко определить свой собственный класс комплексного числа:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Сложение двух комплексных чисел
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Строковое представление для упрощения отладки
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### Использование SciDart для сложных операций

Для более сложных операций или когда производительность критична, `package:scidart` предлагает всеобъемлющую поддержку комплексных чисел среди других функций научных вычислений. Сначала добавьте SciDart в ваш pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Вот как выполнять базовые операции с комплексными числами с помощью SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Создание комплексных чисел
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Сложение
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Умножение
  var product = complexMultiply(complexNum1, complexNum2);

  print('Сумма: ${sum.toString()}');  // Сумма: Complex(real: 7.0, imaginary: 10.0)
  print('Произведение: ${product.toString()}');  // Произведение: Complex(real: -11.0, imaginary: 41.0)
}
```

Эти примеры демонстрируют базовое управление и использование комплексных чисел в Dart, как через пользовательскую реализацию, так и через библиотеку SciDart, подчеркивая гибкость и мощь Dart для задач научных вычислений.
