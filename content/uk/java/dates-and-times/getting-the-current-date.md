---
title:                "Отримання поточної дати"
aliases:
- /uk/java/getting-the-current-date.md
date:                  2024-02-03T19:10:07.544189-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
