---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:07.544189-07:00
description: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 Java \u0454 \u043E\
  \u0441\u043D\u043E\u0432\u043D\u043E\u044E \u043E\u043F\u0435\u0440\u0430\u0446\u0456\
  \u0454\u044E, \u044F\u043A\u0430 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0430\u043C \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u043E\u0431\
  '\u0454\u043A\u0442\u0430\u043C\u0438 \u0434\u0430\u0442\u0438 \u0434\u043B\u044F\
  \ \u043E\u043F\u0435\u0440\u0430\u0446\u0438\u0439, \u0442\u0430\u043A\u0438\u0445\
  \ \u044F\u043A \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F, \u0440\u043E\
  \u0437\u0440\u0430\u0445\u0443\u043D\u043A\u0438\u2026"
lastmod: '2024-03-13T22:44:49.097574-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 Java \u0454 \u043E\
  \u0441\u043D\u043E\u0432\u043D\u043E\u044E \u043E\u043F\u0435\u0440\u0430\u0446\u0456\
  \u0454\u044E, \u044F\u043A\u0430 \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0430\u043C \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u043E\u0431\
  '\u0454\u043A\u0442\u0430\u043C\u0438 \u0434\u0430\u0442\u0438 \u0434\u043B\u044F\
  \ \u043E\u043F\u0435\u0440\u0430\u0446\u0438\u0439, \u0442\u0430\u043A\u0438\u0445\
  \ \u044F\u043A \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F, \u0440\u043E\
  \u0437\u0440\u0430\u0445\u0443\u043D\u043A\u0438\u2026"
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
---

{{< edit_this_page >}}

## Що та Чому?
Отримання поточної дати в Java є основною операцією, яка дозволяє програмістам маніпулювати об'єктами дати для операций, таких як логування, розрахунки дат і умови, засновані на часі. Це життєво важливо в додатках, де відстеження, планування та аналіз тимчасових даних є критичними.

## Як:
Java пропонує кілька способів отримання поточної дати, використовуючи як старий клас `java.util.Date`, так і новіший пакет `java.time` (введений у Java 8), який є більш універсальним і інтуїтивно зрозумілим.

### Використання `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Приклад виводу: 2023-04-01
    }
}
```
### Використання `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Приклад виводу: 2023-04-01T12:34:56.789
    }
}
```
### Використання `java.util.Date` (Застарілий)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Приклад виводу: Сбт Апр 01 12:34:56 BST 2023
    }
}
```
### Використання сторонньої бібліотеки: Joda-Time
До Java 8 Joda-Time була де-факто стандартом для дати та часу в Java. Якщо ви працюєте над застарілими системами або віддаєте перевагу Joda-Time, ось як ви можете використовувати її для отримання поточної дати:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Приклад виводу: 2023-04-01
    }
}
```
**Примітка:** Хоча `java.util.Date` і Joda-Time все ще використовуються, для нових проектів рекомендується пакет `java.time` через його незмінність і всебічний API для обробки дат і часів.
