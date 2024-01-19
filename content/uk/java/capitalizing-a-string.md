---
title:                "Великі літери в рядку"
html_title:           "Java: Великі літери в рядку"
simple_title:         "Великі літери в рядку"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##  Що це та навіщо потрібно?
Великими буквами в Java розуміється перетворення всіх символів рядка на великі букви. Програмісти це роблять для унормованості даних або для порівняння рядків без урахування регістру.

## Як це робити
Метод `toUpperCase()` в Java використовується для перетворення усіх символів в рядку на великі букви. Ось приклад:

```Java 
public class Main {
  public static void main(String[] args) {
    String s = "привіт світе!";
    String s1 = s.toUpperCase();
    System.out.println(s1);
  }
}
```

Виведе: "ПРИВІТ СВІТЕ!"

## Поглиблений аналіз
Рядки в Java незмінні, тому, коли ви викликаєте метод `toUpperCase()`, він створює новий рядок, а старий рядок залишається незмінним. 

Єдиного стандартизованого способу приведення рядків до верхнього регістру не існує. Вибір конкретного методу залежатиме від ваших потреб. Зауважте, що метод `toUpperCase()` має перевагу в універсальності, але може стати проблемою, коли працюєте з рядками невеликих розмірів у великих об'ємах через невеликі, але кумулятивні витрати ресурсів.

## Дивіться також 
- Java String toUpperCase(Locale locale) Method: [https://www.javatpoint.com/java-string-touppercase](https://www.javatpoint.com/java-string-touppercase)
- The Java String Class API: [https://docs.oracle.com/javase/7/docs/api/java/lang/String.html](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)