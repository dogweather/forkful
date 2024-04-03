---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:36.727454-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\
  \u043F\u043E\u0441\u043E\u0431 \u043D\u0430\u0447\u0430\u0442\u044C \u0440\u0430\
  \u0431\u043E\u0442\u0443 \u0441 \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C \u0432 Java, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0439 \u043F\
  \u0430\u043A\u0435\u0442 `java.util.logging`."
lastmod: '2024-03-13T22:44:44.833655-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441\u043F\
  \u043E\u0441\u043E\u0431 \u043D\u0430\u0447\u0430\u0442\u044C \u0440\u0430\u0431\
  \u043E\u0442\u0443 \u0441 \u043B\u043E\u0433\u0438\u0440\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u0432 Java, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0439 \u043F\u0430\
  \u043A\u0435\u0442 `java.util.logging`."
title: "\u0416\u0443\u0440\u043D\u0430\u043B\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435"
weight: 17
---

## Как это сделать:
Вот простой способ начать работу с логированием в Java, используя встроенный пакет `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Логирование сообщения уровня INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Произошло исключение", e);
        }
    }
}
```

Это приведёт к выводу вида:

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: Логирование сообщения уровня INFO
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: Произошло исключение
java.lang.ArithmeticException: / на ноль
    at AppLogging.main(AppLogging.java:10)
```

## Подробнее
Логирование в Java значительно развилось. Исторически, логирование было более адаптивным с системными выводами и самописными механизмами. Однако потребность в стандартизации привела к появлению API для логирования, таких как `Log4j` и `SLF4J`. Пакет `java.util.logging` был введен в JDK 1.4, предоставляя стандартизированный способ записи сообщений.

Альтернативы `java.util.logging` (JUL) включают Log4j 2 и SLF4J. Хотя JUL встроен в Java и поэтому не требует дополнительных зависимостей, как Log4j 2 и SLF4J предлагают более продвинутые функции, такие как более детальный контроль над конфигурацией логирования, асинхронное логирование и лучшая производительность.

С точки зрения реализации, логирование может быть как синхронным, когда каждое сообщение лога обрабатывается в том потоке, который его сгенерировал, так и асинхронным, когда сообщения передаются в отдельный поток. Асинхронное логирование может улучшить производительность, но вводит сложность, поскольку необходимо управлять параллелизмом и гарантировать, что сообщения лога не будут потеряны при сбое приложения.

## Смотрите также
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Официальный обзор логирования от Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Учебник по java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
