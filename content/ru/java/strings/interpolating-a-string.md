---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:20.529252-07:00
description: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\
  \u0442 \u043D\u0430\u043F\u0440\u044F\u043C\u0443\u044E \u0432\u0441\u0442\u0430\
  \u0432\u043B\u044F\u0442\u044C \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\
  \u0435 \u0432 \u0441\u0442\u0440\u043E\u043A\u0438. \u042D\u0442\u043E \u0434\u0435\
  \u043B\u0430\u0435\u0442 \u043A\u043E\u0434 \u0431\u043E\u043B\u0435\u0435 \u0447\
  \u0438\u0441\u0442\u044B\u043C \u0438 \u043B\u0435\u0433\u043A\u0438\u043C \u0434\
  \u043B\u044F \u0447\u0442\u0435\u043D\u0438\u044F, \u0438\u0437\u0431\u0435\u0433\
  \u0430\u044F \u0433\u0440\u043E\u043C\u043E\u0437\u0434\u043A\u043E\u0439 \u043A\
  \u043E\u043D\u043A\u0430\u0442\u0435\u043D\u0430\u0446\u0438\u0438\u2026"
lastmod: '2024-03-13T22:44:44.793838-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F\
  \ \u0441\u0442\u0440\u043E\u043A \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\
  \u0442 \u043D\u0430\u043F\u0440\u044F\u043C\u0443\u044E \u0432\u0441\u0442\u0430\
  \u0432\u043B\u044F\u0442\u044C \u043F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\u044B\
  \u0435 \u0432 \u0441\u0442\u0440\u043E\u043A\u0438."
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 8
---

## Как это сделать:
Java ввела `String.format()` для интерполяции:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Алиса";
    int points = 1337;
    String greeting = String.format("Привет, %s! У тебя %d очков.", user, points);
    System.out.println(greeting);
  }
}
```
Пример вывода:
```
Привет, Алиса! У тебя 1337 очков.
```

Для более современной интерполяции начиная с Java 15 используются текстовые блоки и `formatted()`:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Боб";
    double accountBalance = 1234.56;
    String message = """
      Уважаемый %s,
      Ваш текущий баланс составляет $%.2f.
      """.formatted(user, accountBalance);
    System.out.println(message);
  }
}
```
Пример вывода:
```
Уважаемый Боб,
Ваш текущий баланс составляет $1234.56.
```

## Подробнее
До интерполяции Java использовала конкатенацию: `String greeting = "Привет, " + user + "!";`. Это было громоздко и подвержено ошибкам, особенно когда строки становились сложными.

Исторически, такие языки, как Perl и PHP, имели интерполяцию. Java догнала их гораздо позже. `String.format()` и `PrintStream.printf()` предлагают аналогичную функциональность, используя спецификаторы формата, которые говорят Java, как обрабатывать переменные.

Альтернативы? Кроме `String.format()`, у нас есть `MessageFormat` и `StringBuilder`, но они не так удобны для базовой интерполяции. Начиная с Java 15, текстовые блоки упростили многострочные строки и добавили `formatted()` для упрощения интерполяции непосредственно на месте.

С точки зрения реализации, `String.format()` использует `Formatter`, мощный движок с множеством опций форматирования. Но будьте осторожны, сложные строки могут снизить производительность вашего приложения, если вы не будете осторожны.

## Смотрите также
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Formatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [JEP 378: Текстовые блоки (Окончательно)](https://openjdk.java.net/jeps/378)
