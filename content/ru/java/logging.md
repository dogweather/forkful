---
title:                "Журналирование"
date:                  2024-01-28T23:59:36.727454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Журналирование"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Логирование – это фактически процесс записи событий, происходящих внутри программного приложения. Программисты регистрируют эти события для захвата информации во время выполнения, отладки проблем, мониторинга поведения системы и создания аудита для целей безопасности и соответствия требованиям.

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
