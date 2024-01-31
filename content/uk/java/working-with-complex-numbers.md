---
title:                "Робота з комплексними числами"
date:                  2024-01-26T04:43:59.537889-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"

category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що та Чому?

Комплексні числа розширюють ряд реальних чисел за рахунок додавання уявної одиниці, `i`, де `i^2 = -1`. Вони відіграють ключову роль у таких сферах, як інженерія, фізика та складна математика, де моделюють явища, з якими реальні числа не можуть впоратися, наприклад, електричні струми та обробка сигналів.

## Як це зробити:

Java не має вбудованої підтримки комплексних чисел, але ми можемо створити власний клас або використати бібліотеку. Ось швидкий приклад того, як створити простий клас `ComplexNumber` і використовувати його:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString для відображення комплексних чисел у форматі a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Швидкий тест
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Сума: " + c1.add(c2));
    }
}
```

Приклад виводу для головного методу буде:

```
Сума: 3.0 + 7.0i
```

## Поглиблений Огляд

Перед високорівневими мовами, як-от Java, програмісти працювали безпосередньо з математичними бібліотеками на мовах, як-от Fortran або C, для управління складними операціями. Концепція сягає 16 століття, заслуговуючи визнання математиків, таких як Джироламо Кардано та Рафаель Бомбеллі.

В Java, `java.lang.Math` є місцем для суттєвих потреб, але пропускає комплексні числа, ймовірно, тому що не кожен програміст їх використовує. Альтернативи? Використання бібліотек. Apache Commons Math надає клас `Complex`, наповнений методами для маніпуляцій. Ось чому створення власного класу є чудовим: Легкість, налаштування під конкретні потреби та відсутність накладних витрат на бібліотеку.

Одна важлива деталь: увага на точність чисел з плаваючою комою. Комп'ютери не можуть точно представити деякі числа, що призводить до помилок округлення. При виконанні повторюваних складних операцій, ці помилки можуть накопичуватися!

## Дивіться також

Для поглибленого вивчення та більш складних операцій, перегляньте:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [Комплексний клас JScience](http://jscience.org/)
- Посібники Oracle по [арифметиці чисел з плаваючою комою](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
