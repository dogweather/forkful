---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:07.675510-07:00
description: "\u041A\u0430\u043A \u0440\u0430\u0431\u043E\u0442\u0430\u0442\u044C\
  : \u0412 Java \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\
  \u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u043A\u043E\
  \u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B\
  , \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\u043C \u0441\u043E\u0437\u0434\
  \u0430\u0442\u044C \u0441\u0432\u043E\u0439 \u0441\u043E\u0431\u0441\u0442\u0432\
  \u0435\u043D\u043D\u044B\u0439 \u043A\u043B\u0430\u0441\u0441 \u0438\u043B\u0438\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443. \u0412\u043E\u0442 \u0431\
  \u044B\u0441\u0442\u0440\u044B\u0439\u2026"
lastmod: '2024-03-13T22:44:44.808735-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Java \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u043A\
  \u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\
  \u043B, \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\u043C \u0441\u043E\u0437\
  \u0434\u0430\u0442\u044C \u0441\u0432\u043E\u0439 \u0441\u043E\u0431\u0441\u0442\
  \u0432\u0435\u043D\u043D\u044B\u0439 \u043A\u043B\u0430\u0441\u0441 \u0438\u043B\
  \u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как работать:
В Java нет встроенной поддержки комплексных чисел, но мы можем создать свой собственный класс или использовать библиотеку. Вот быстрый пример того, как создать простой класс `ComplexNumber` и использовать его:

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

    // ToString для отображения комплексных чисел в форме a + bi
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Быстрый тест
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Сумма: " + c1.add(c2));
    }
}
```

Пример вывода для основного метода будет:

```
Сумма: 3.0 + 7.0i
```

## Подробнее
До появления высокоуровневых языков, таких как Java, программисты работали непосредственно с математическими библиотеками на языках, таких как Fortran или C, чтобы управлять комплексными операциями. Концепция восходит к 16 веку, за что благодарность принадлежит математикам вроде Джероламо Кардано и Рафаэля Бомбелли.

В Java `java.lang.Math` является основой для всего необходимого, но пропускает комплексные числа, возможно, потому что не каждый программист их использует. Альтернативы? Использовать библиотеки. Apache Commons Math предоставляет класс `Complex`, насыщенный методами для манипуляции. Вот почему создание своего собственного класса может быть удобно: легковесность, точное соответствие вашим потребностям, и отсутствие накладных расходов библиотеки.

Один важный момент: следите за точностью с плавающей точкой. Компьютеры не могут точно представить некоторые числа, что приводит к ошибкам округления. При выполнении повторяющихся комплексных операций эти ошибки могут накапливаться!

## См. также
Для более глубокого изучения и более сложных операций обратите внимание:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [Complex class от JScience](http://jscience.org/)
- Учебные пособия Oracle по [арифметике с плавающей точкой](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
