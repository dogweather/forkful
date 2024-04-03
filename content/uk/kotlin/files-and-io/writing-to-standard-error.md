---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:22.038807-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Kotlin \u0437\u0430\u043F\u0438\u0441 \u0432 stderr \u043C\u043E\u0436\u043D\
  \u0430 \u0437\u0434\u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 `System.err.println()`.\
  \ \u0426\u0435\u0439 \u043C\u0435\u0442\u043E\u0434 \u0441\u0445\u043E\u0436\u0438\
  \u0439 \u043D\u0430 `System.out.println()`, \u0430\u043B\u0435 \u0441\u043F\u0440\
  \u044F\u043C\u043E\u0432\u0443\u0454\u2026"
lastmod: '2024-03-13T22:44:49.246494-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Kotlin \u0437\u0430\u043F\u0438\u0441 \u0432 stderr \u043C\u043E\u0436\
  \u043D\u0430 \u0437\u0434\u0456\u0439\u0441\u043D\u0438\u0442\u0438 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 `System.err.println()`."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

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
