---
title:                "Запис до стандартної помилки"
aliases: - /uk/java/writing-to-standard-error.md
date:                  2024-02-03T19:34:09.861880-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запис до стандартної помилки"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Запис до стандартного потоку помилок (stderr) включає виведення повідомлень про помилки та діагностику на консоль або термінал. Програмісти роблять це, щоб відокремити інформацію про помилки від стандартного виводу (stdout), що спрощує налагодження та аналіз логів.

## Як це зробити:

### Базовий вивід до stderr в Java
Java надає простий спосіб запису в stderr використовуючи `System.err.print()` або `System.err.println()`. Ось як це робиться:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Помилка: Неможливо поділити на нуль.");
        }
    }
}
```

Приклад виводу:

```
Помилка: Неможливо поділити на нуль.
```

Це безпосередньо виведе повідомлення про помилку в стандартний потік помилок.

### Використання логера для розширеного оброблення помилок
Для додатків, яким потрібне більш витончене оброблення помилок та логування, поширено користування бібліотеками логування, такими як SLF4J з Logback або Log4J2. Це дозволяє більш гнучко керувати виводом помилок, включаючи перенаправлення файлів, фільтрацію та форматування.

#### Приклад з Logback

Спочатку додайте залежність для Logback до файлу `pom.xml` (Maven) або `build.gradle` (Gradle). Для Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Потім ви можете використовувати наступний код для логування помилок:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Помилка: Неможливо поділити на нуль.", e);
        }
    }
}
```

Це виведе повідомлення про помилку разом зі стеком викликів на консоль або в файл, залежно від конфігурації Logback.

Користування фреймворками для логування, такими як Logback, надає більший контроль над обробленням помилок, що полегшує керування великими додатками і системами.
