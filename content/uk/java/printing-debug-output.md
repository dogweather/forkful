---
title:    "Java: Виведення відладкового виводу"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/printing-debug-output.md"
---

{{< edit_this_page >}}

### Чому

Debug виведення є важливою частиною програмування в Java. Воно дозволяє відстежувати роботу коду, знаходити та виправляти помилки, що зробить вашу програму більш надійною та ефективною.

### Як

```Java
public class DebugExample {
    public static void main(String[] args) {
        int num1 = 10;
        int num2 = 5;
        int sum = num1 + num2;
        System.out.println("Сума двох чисел: " + sum);
    }
}
```

В даному прикладі ми використовуємо оператор додавання `+` для об'єднання рядка та змінної `sum` у виведенні результату. Після запуску програми, у вас буде виведено наступне:

`Сума двох чисел: 15`

Це дозволяє нам перевірити, чи правильно була обчислена сума двох чисел.

### Глибше

Окрім простого виведення змінних, ви також можете використовувати Debug виведення для відстеження роботи циклів, методів та умовних операторів. Також можна використовувати Debug виведення для виведення значень згенерованих помилок, що допоможе вам знайти та виправити їх.

### Дивись також

- [Документація Oracle: використання Debug виведення](https://www.oracle.com/java/technologies/javase/debug.html)
- [Стаття на JavaCodeGeeks про Debug виведення](https://www.javacodegeeks.com/2013/04/java-debugging-tutorial-how-to-debug-java-program.html)
- [Відео українською про Debug виведення в Java](https://www.youtube.com/watch?v=73Z4xER4V7o)