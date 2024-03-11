---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:58.505830-07:00
description: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF08\u901A\
  \u5E38\u8868\u793A\u4E3Aa + bi\uFF09\uFF0C\u5C06\u65E0\u7EF4\u6570\u7684\u6982\u5FF5\
  \u6269\u5C55\u5230\u4E8C\u7EF4\u7A7A\u95F4\u3002\u5728\u7535\u6C14\u5DE5\u7A0B\u3001\
  \u91CF\u5B50\u8BA1\u7B97\u548C\u6D41\u4F53\u52A8\u529B\u5B66\u7B49\u9886\u57DF\uFF0C\
  \u7A0B\u5E8F\u5458\u4F7F\u7528\u590D\u6570\u6765\u6A21\u62DF\u65E0\u6CD5\u4EC5\u7528\
  \u5B9E\u6570\u5355\u4E00\u7EF4\u5EA6\u8868\u793A\u7684\u73B0\u8C61\u3002"
lastmod: '2024-03-11T00:14:21.168312-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF08\u901A\
  \u5E38\u8868\u793A\u4E3Aa + bi\uFF09\uFF0C\u5C06\u65E0\u7EF4\u6570\u7684\u6982\u5FF5\
  \u6269\u5C55\u5230\u4E8C\u7EF4\u7A7A\u95F4\u3002\u5728\u7535\u6C14\u5DE5\u7A0B\u3001\
  \u91CF\u5B50\u8BA1\u7B97\u548C\u6D41\u4F53\u52A8\u529B\u5B66\u7B49\u9886\u57DF\uFF0C\
  \u7A0B\u5E8F\u5458\u4F7F\u7528\u590D\u6570\u6765\u6A21\u62DF\u65E0\u6CD5\u4EC5\u7528\
  \u5B9E\u6570\u5355\u4E00\u7EF4\u5EA6\u8868\u793A\u7684\u73B0\u8C61\u3002"
title: "\u5904\u7406\u590D\u6570\u95EE\u9898"
---

{{< edit_this_page >}}

## 什么以及为什么？

复数由实部和虚部组成（通常表示为a + bi），将无维数的概念扩展到二维空间。在电气工程、量子计算和流体动力学等领域，程序员使用复数来模拟无法仅用实数单一维度表示的现象。

## 如何操作：

Dart本身不包含用于复数的内建库，这就需要实现一个自定义的复数类或使用第三方库。`package:scidart` 是一个用于科学计算任务的流行选择，它提供了对复数的支持。

### 实现一个基本的复数类

对于简单操作，你可以轻松定义自己的复数类：

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // 两个复数的加法
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // 方便调试的字符串表示
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

### 使用SciDart进行高级操作

对于更复杂的操作或当性能至关重要时，`package:scidart` 提供了包括复数在内的其他科学计算功能的全面支持。首先，将SciDart添加到你的pubspec.yaml中：

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

这是使用SciDart进行复数基本操作的方法：

```dart
import 'package:scidart/numdart.dart';

void main() {
  // 创建复数
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // 加法
  var sum = complexAdd(complexNum1, complexNum2);
  
  // 乘法
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // 和: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // 积: Complex(real: -11.0, imaginary: 41.0)
}
```

这些例子展示了在Dart中基本操作和利用复数的方式，通过自定义实现和通过SciDart库，凸显了Dart在科学计算任务中的灵活性和强大功能。
