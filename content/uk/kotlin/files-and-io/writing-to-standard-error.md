---
title:                "Запис до стандартної помилки"
date:                  2024-02-03T19:34:22.038807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Запис в стандартний потік помилок (stderr) полягає у виведенні повідомлень про помилки та діагностичної інформації у відокремлений потік, що відрізняється від стандартного виводу (stdout). Це дозволяє краще обробляти помилки та аналізувати журнали. Програмісти роблять це для того, щоб полегшити налагодження та забезпечити легке ідентифікацію та перенаправлення повідомлень про помилки за потреби, підтримуючи чистоту журналів виводу або повідомлень для користувачів.

## Як це зробити:

У Kotlin запис в stderr можна здійснити використовуючи `System.err.println()`. Цей метод схожий на `System.out.println()`, але спрямовує вивід до стандартного потоку помилок, а не до стандартного потоку виводу.

```kotlin
fun main() {
    System.err.println("Це повідомлення про помилку!")
}
```

Приклад виводу:
```
Це повідомлення про помилку!
```

Для більш структурованих або складних додатків, особливо тих, що використовують фреймворки для логування, такі як Logback або SLF4J, можна налаштувати логери на запис в stderr для певних рівнів логування (наприклад, ERROR).

Використання SLF4J із Logback:

1. Спочатку додайте API SLF4J та імплементацію Logback до вашого `build.gradle`:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. Далі налаштуйте Logback (в `src/main/resources/logback.xml`), щоб спрямовувати повідомлення рівня помилки до stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. Потім використовуйте SLF4J у вашому коді Kotlin для логування повідомлень про помилки:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("Це повідомлення логу помилки!")
}
```

Приклад виводу (в stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - Це повідомлення логу помилки!
```
