---
title:                "Логування"
date:                  2024-01-26T01:07:23.505161-07:00
model:                 gpt-4-1106-preview
simple_title:         "Логування"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/logging.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Логування - це процес запису подій, що відбуваються в рамках програмного забезпечення. Програмісти ведуть лог подій, щоб фіксувати інформацію виконання програм, відлагоджувати проблеми, моніторити поведінку системи та створювати журнал аудиту для цілей безпеки та відповідності.

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
