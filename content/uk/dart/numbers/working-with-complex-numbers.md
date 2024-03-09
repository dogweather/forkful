---
title:                "Робота з комплексними числами"
date:                  2024-03-08T21:58:12.672822-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Комплексні числа, що складаються з дійсної і уявної частини (зазвичай позначаються як a + bi), розширюють концепцію безрозмірних чисел до двовимірного простору. Програмісти працюють з комплексними числами у таких галузях, як електротехніка, квантові обчислення та гідродинаміка для моделювання явищ, які не можуть бути представлені в межах одного виміру дійсних чисел.

## Як це зробити:

Dart сам по собі не включає вбудовану бібліотеку для комплексних чисел, що потребує або реалізації власного класу комплексних чисел, або використання сторонньої бібліотеки. Популярним вибором для наукових обчислень, який включає підтримку комплексних чисел, є `package:scidart`.

### Реалізація Базового Класу Комплексних Чисел

Для простих операцій ви легко можете визначити власний клас комплексних чисел:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Додавання двох комплексних чисел
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Рядкове представлення для легкого відлагодження
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

### Використання SciDart для Просунутих Операцій

Для більш складних операцій або коли продуктивність є критичною, `package:scidart` пропонує комплексну підтримку комплексних чисел серед інших функціональностей наукових обчислень. Спочатку додайте SciDart у ваш pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Ось як виконати базові операції з комплексними числами за допомогою SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Створення комплексних чисел
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Додавання
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Множення
  var product = complexMultiply(complexNum1, complexNum2);

  print('Сума: ${sum.toString()}');  // Сума: Complex(real: 7.0, imaginary: 10.0)
  print('Добуток: ${product.toString()}');  // Добуток: Complex(real: -11.0, imaginary: 41.0)
}
```

Ці приклади демонструють базову маніпуляцію та використання комплексних чисел в Dart, як через власну реалізацію, так і через бібліотеку SciDart, підкреслюючи гнучкість і потужність Dart для наукових обчислень.
