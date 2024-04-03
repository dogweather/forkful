---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:09.861880-07:00
description: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u043E\u0433\u043E \u043F\u043E\u0442\u043E\u043A\u0443\
  \ \u043F\u043E\u043C\u0438\u043B\u043E\u043A (stderr) \u0432\u043A\u043B\u044E\u0447\
  \u0430\u0454 \u0432\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043F\u043E\
  \u0432\u0456\u0434\u043E\u043C\u043B\u0435\u043D\u044C \u043F\u0440\u043E \u043F\
  \u043E\u043C\u0438\u043B\u043A\u0438 \u0442\u0430 \u0434\u0456\u0430\u0433\u043D\
  \u043E\u0441\u0442\u0438\u043A\u0443 \u043D\u0430 \u043A\u043E\u043D\u0441\u043E\
  \u043B\u044C \u0430\u0431\u043E \u0442\u0435\u0440\u043C\u0456\u043D\u0430\u043B\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435,\u2026"
lastmod: '2024-03-13T22:44:49.107355-06:00'
model: gpt-4-0125-preview
summary: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0433\u043E \u043F\u043E\u0442\u043E\u043A\u0443\
  \ \u043F\u043E\u043C\u0438\u043B\u043E\u043A (stderr) \u0432\u043A\u043B\u044E\u0447\
  \u0430\u0454 \u0432\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043F\u043E\
  \u0432\u0456\u0434\u043E\u043C\u043B\u0435\u043D\u044C \u043F\u0440\u043E \u043F\
  \u043E\u043C\u0438\u043B\u043A\u0438 \u0442\u0430 \u0434\u0456\u0430\u0433\u043D\
  \u043E\u0441\u0442\u0438\u043A\u0443 \u043D\u0430 \u043A\u043E\u043D\u0441\u043E\
  \u043B\u044C \u0430\u0431\u043E \u0442\u0435\u0440\u043C\u0456\u043D\u0430\u043B\
  ."
title: "\u0417\u0430\u043F\u0438\u0441 \u0434\u043E \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u043E\u0457 \u043F\u043E\u043C\u0438\u043B\u043A\u0438"
weight: 25
---

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
