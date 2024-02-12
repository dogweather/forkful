---
title:                "Работа с комплексными числами"
aliases: - /ru/java/working-with-complex-numbers.md
date:                  2024-01-29T00:06:07.675510-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с комплексными числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/working-with-complex-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Комплексные числа расширяют вещественную числовую ось за счёт добавления мнимой единицы, `i`, где `i^2 = -1`. Они имеют решающее значение в таких областях, как инженерия, физика и продвинутая математика, где моделируют явления, с которыми вещественные числа не справляются, например, электрические токи и обработка сигналов.

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
