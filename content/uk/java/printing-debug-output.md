---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:48.848689-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Виведення відлагоджувальної інформації — це, коротко кажучи, виведення тексту, який допомагає розробникам побачити, що відбувається у коді під час його виконання. Ми робимо це, щоб легко знайти та виправити помилки — часто використовуючи `System.out.println()` у Java.

## Як це робити:

```Java
public class DebugExample {
    public static void main(String[] args) {
        int addend1 = 5;
        int addend2 = 7;
        int sum = addend1 + addend2;

        // Вивід для відлагодження
        System.out.println("Додаємо: " + addend1 + " + " + addend2);
        System.out.println("Результат: " + sum);
    }
}
```

Sample output:

```
Додаємо: 5 + 7
Результат: 12
```

## Поглиблено:

Історично, з появою мов програмування було важливо мати засоби для визначення проблем у програмах, тому виведення відлагоджувальної інформації стало стандартною практикою. У Java, стандартний потік виводу (sysout) був і залишається основним інструментом. Альтернативи включають використання журналувальних фреймворків, таких як Log4j чи SLF4J, які надають більше контролю і опцій для різних рівнів відлагоджувального виводу (info, debug, warn, error). Щодо реалізації, `System.out` використовує `PrintStream`, що надає не тільки `println()`, а й методи типу `print()` та `printf()` для форматованого виводу. У багатопоточних додатках важливо також зазначити, що виведення через `System.out` може бути неатомічним, і, отже, потребує правильної синхронізації.

## Дивись також:

- Oracle Java Documentation: [System.out.println](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/io/PrintStream.html#println())
- Apache Log4j: [Official Website](https://logging.apache.org/log4j/)
- The SLF4J Project: [Official Website](http://www.slf4j.org/)
- Oracle tutorial on logging: [Java Logging Overview](https://docs.oracle.com/javase/tutorial/essential/logging/)