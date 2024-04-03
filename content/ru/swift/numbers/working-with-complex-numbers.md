---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:26.212738-07:00
description: "\u041A\u0430\u043A: \u0412 Swift \u043D\u0435\u0442 \u0432\u0441\u0442\
  \u0440\u043E\u0435\u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\
  \u043A\u0438 \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445\
  \ \u0447\u0438\u0441\u0435\u043B, \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\
  \u043C \u0441\u043E\u0437\u0434\u0430\u0442\u044C \u0441\u0432\u043E\u044E."
lastmod: '2024-03-13T22:44:45.672559-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Swift \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u043A\
  \u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\
  \u043B, \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\u043C \u0441\u043E\u0437\
  \u0434\u0430\u0442\u044C \u0441\u0432\u043E\u044E."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как:
В Swift нет встроенной поддержки комплексных чисел, но мы можем создать свою:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Дополнительные методы, такие как вычитание, умножение и т.д.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Результат: \(result.real) + \(result.imaginary)i")
// Пример вывода: Результат: 3.0 + 7.0i
```

## Глубже
Комплексные числа появились в 16-м веке в алгебраических уравнениях. Они необходимы в квантовой механике, теории управления и многих других областях. У Apple Swift нет стандартной библиотеки для комплексных чисел, в отличие от языков, таких как Python или C++. Альтернативы созданию собственной реализации включают использование пакета Numerics, который поддерживает комплексные числа, или интеграцию библиотеки комплексных чисел C++ с помощью взаимодействия Swift.

## Смотрите также
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
