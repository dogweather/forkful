---
date: 2024-01-20 17:52:48.848689-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0406\
  \u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E, \u0437 \u043F\u043E\u044F\u0432\
  \u043E\u044E \u043C\u043E\u0432 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\
  \u0432\u0430\u043D\u043D\u044F \u0431\u0443\u043B\u043E \u0432\u0430\u0436\u043B\
  \u0438\u0432\u043E \u043C\u0430\u0442\u0438 \u0437\u0430\u0441\u043E\u0431\u0438\
  \ \u0434\u043B\u044F \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F\
  \ \u043F\u0440\u043E\u0431\u043B\u0435\u043C \u0443 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0430\u0445, \u0442\u043E\u043C\u0443 \u0432\u0438\u0432\u0435\u0434\
  \u0435\u043D\u043D\u044F \u0432\u0456\u0434\u043B\u0430\u0433\u043E\u0434\u0436\u0443\
  \u0432\u0430\u043B\u044C\u043D\u043E\u0457\u2026"
lastmod: '2024-04-05T22:51:02.199538-06:00'
model: gpt-4-1106-preview
summary: "\u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E, \u0437 \u043F\u043E\
  \u044F\u0432\u043E\u044E \u043C\u043E\u0432 \u043F\u0440\u043E\u0433\u0440\u0430\
  \u043C\u0443\u0432\u0430\u043D\u043D\u044F \u0431\u0443\u043B\u043E \u0432\u0430\
  \u0436\u043B\u0438\u0432\u043E \u043C\u0430\u0442\u0438 \u0437\u0430\u0441\u043E\
  \u0431\u0438 \u0434\u043B\u044F \u0432\u0438\u0437\u043D\u0430\u0447\u0435\u043D\
  \u043D\u044F \u043F\u0440\u043E\u0431\u043B\u0435\u043C \u0443 \u043F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0430\u0445, \u0442\u043E\u043C\u0443 \u0432\u0438\u0432\
  \u0435\u0434\u0435\u043D\u043D\u044F \u0432\u0456\u0434\u043B\u0430\u0433\u043E\u0434\
  \u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\u0444\u043E\u0440\
  \u043C\u0430\u0446\u0456\u0457 \u0441\u0442\u0430\u043B\u043E \u0441\u0442\u0430\
  \u043D\u0434\u0430\u0440\u0442\u043D\u043E\u044E \u043F\u0440\u0430\u043A\u0442\u0438\
  \u043A\u043E\u044E."
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

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
