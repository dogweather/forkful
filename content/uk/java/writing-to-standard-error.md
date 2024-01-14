---
title:                "Java: Писання до стандартного помилкового потоку"
simple_title:         "Писання до стандартного помилкового потоку"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Why (Чому)

Написання у стандартний потік помилок (stderr) може бути корисним для виявлення та відлагодження помилок у вашому коді. Використання цього потоку допомагає уникнути неправильних результатів та забезпечує більш точне виявлення проблем у програмі.

How To (Як це зробити)

Використовуючи Java, ми можемо направляти вивід до stderr за допомогою методу System.err.println(). Приклад коду:
```Java
public class Demo {
    public static void main(String[] args) {
        System.err.println("Повідомлення про помилку");
    }
}
```
Вихідний код буде наступним:
```bash
Повідомлення про помилку
```

Deep Dive (Детальніше)

Крім методу System.err.println(), також існує можливість додати додаткові інформаційні повідомлення до stdout (стандартний потік виводу). Для цього можна використовувати метод System.out.println() з подальшим використанням методу System.setOut(). На прикладі коду це виглядатиме так:
```Java
public class Demo {
    public static void main(String[] args) {
        System.out.println("Відлагодження...");
        // Використання System.setOut()
        System.setOut(System.err);
        System.out.println("Повідомлення про помилку");
    }
}
```
Виходячи з цього прикладу, повідомлення про помилку буде виводитися разом з повідомленням для відлагодження. Це дозволяє збільшити читабельність і зрозумілість виходу.

See Also (Див. також)

- [Документація Java про метод System.err.println()](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Порівняння System.out та System.err в Java](https://stackoverflow.com/questions/8128302/java-why-would-you-output-to-system-err)
- [Як використовувати стандартний потік помилок для відлагодження в Java](https://www.logicbig.com/tutorials/core-java-tutorial/java-io/standard-error-output.html)