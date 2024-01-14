---
title:    "Java: Написання тестів"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною процесу розробки програмного забезпечення, оскільки допомагає виявити можливі помилки та забезпечити стабільність програми. Тестування також зменшує ризик виникнення проблем у продукції та допомагає швидше виявляти та виправляти їх.

## Як

Нижче наведені приклади коду та вихідні дані для написання тестів у Java.

```Java
// Приклад класу, який будемо тестувати
public class Calculator {
    // Метод для додавання двох чисел
    public int add(int num1, int num2) {
        return num1 + num2;
    }
}

// Приклад файлу тестів
public class CalculatorTest {
    // Ініціалізація об'єкта класу Calculator
    private Calculator calculator = new Calculator();

    // Метод для тестування методу add
    @Test
    public void testAdd() {
        // Вірогідні параметри та очікуваний результат
        int num1 = 5;
        int num2 = 7;
        int expected = 12;

        // Виклик методу та перевірка результату
        int result = calculator.add(num1, num2);
        assertEquals(expected, result);
    }
}
```

Вихідні дані з тесту: `12`

## Глибоке погруження

Написання ефективних тестів вимагає від програміста розуміння принципів їх роботи та вигоди для проекту. Деякі з основних принципів написання тестів у Java:

- Юніт-тести повинні бути писаними для окремих функцій та методів, а не для всієї програми цілком.
- Використовуйте позитивні та негативні тести для перевірки усіх випадків.
- Перевіряйте коректність вхідних даних та очікуваного результату.
- Виокремлюйте загальне кодування для тестів у окремі класи та методи.

## Дивіться також

- [JUnit documentation](https://junit.org/junit5/docs/current/user-guide/)
- [Test driven development in Java](https://www.javaworld.com/article/2073171/test-driven-development-in-java.html)
- [The benefits of testing in software development](https://stackify.com/benefit-testing-software-development/)