---
date: 2024-01-26 01:07:23.505161-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\u043E\
  \u0441\u0456\u0431 \u043F\u043E\u0447\u0430\u0442\u0438 \u043B\u043E\u0433\u0443\
  \u0432\u0430\u043D\u043D\u044F \u0432 Java, \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0438\u0439 \u043F\u0430\u043A\u0435\u0442 `java.util.logging`."
lastmod: '2024-03-13T22:44:49.090625-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u0441\u043F\
  \u043E\u0441\u0456\u0431 \u043F\u043E\u0447\u0430\u0442\u0438 \u043B\u043E\u0433\
  \u0443\u0432\u0430\u043D\u043D\u044F \u0432 Java, \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0432\u0431\u0443\u0434\u043E\
  \u0432\u0430\u043D\u0438\u0439 \u043F\u0430\u043A\u0435\u0442 `java.util.logging`."
title: "\u041B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F"
weight: 17
---

## Як це зробити:
Ось простий спосіб почати логування в Java, використовуючи вбудований пакет `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Logging an INFO-level message");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Exception occur", e);
        }
    }
}
```

Це призведе до наступного виводу:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Logging an INFO-level message
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Exception occur
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Поглиблений дайвінг
Логування в Java зазнало чимало змін. Історично логування було більше ад-хок з системними виводами та механізмами, написаними самостійно. Однак потреба у стандартизації призвела до появи API логування, таких як `Log4j` та `SLF4J`. Пакет `java.util.logging` був введений у JDK 1.4, надаючи стандартизований спосіб запису повідомлень.

Альтернативи для `java.util.logging` (JUL) включають Log4j 2 та SLF4J. Перевагою JUL є те, що вона вбудована в Java і тому не вимагає додаткових залежностей, однак і Log4j 2, і SLF4J пропонують більш передові можливості, такі як більш детальне управління конфігурацією логування, асинхронне логування та кращу продуктивність.

З точки зору реалізації, логування може бути синхронним, коли кожне повідомлення обробляється в потоці, який його згенерував, або асинхронним, коли повідомлення передаються в окремий потік. Асинхронне логування може покращити продуктивність, але вносить складність, оскільки потрібно обробляти одночасність і забезпечити, щоб повідомлення логів не загубилися при збої програми.

## Дивіться також
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Офіційний огляд логування від Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Підручник по java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
