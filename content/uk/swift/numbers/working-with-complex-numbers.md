---
title:                "Робота з комплексними числами"
aliases:
- /uk/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:46:13.894049-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що і Чому?
Комплексні числа складаються з дійсної та уявної частини (наприклад, 3 + 4i). Програмісти використовують їх у Swift для завдань, таких як обробка сигналів, розв’язання певних математичних проблем та симуляція фізики.

## Як це зробити:
Swift не має вбудованої підтримки комплексних чисел, але ми можемо створити власну:

```Swift
struct ComplexNumber {
    var real: Double
    var imaginary: Double
    
    func add(_ other: ComplexNumber) -> ComplexNumber {
        return ComplexNumber(real: real + other.real, imaginary: imaginary + other.imaginary)
    }
    
    // Додаткові методи, як-от віднімання, множення тощо.
}

let first = ComplexNumber(real: 2, imaginary: 3)
let second = ComplexNumber(real: 1, imaginary: 4)
let result = first.add(second)
print("Результат: \(result.real) + \(result.imaginary)i")
// Приклад виводу: Результат: 3.0 + 7.0i
```

## Поглиблено
Комплексні числа з'явилися у 16 столітті у алгебраїчних рівняннях. Вони є життєво важливими в квантовій механіці, теорії керування та багатьох інших областях. Apple's Swift не має стандартної бібліотеки для комплексних чисел, на відміну від мов як Python чи C++. Альтернативи створенню власної включають використання пакету Numerics, який підтримує комплексні числа, або обгортання C++ комплексної бібліотеки за допомогою взаємодії зі Swift.

## Дивіться також
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
