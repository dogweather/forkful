---
title:                "Розрахунок дати у майбутньому або минулому"
html_title:           "Java: Розрахунок дати у майбутньому або минулому"
simple_title:         "Розрахунок дати у майбутньому або минулому"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Що і чому?
Обчислення дати в майбутньому або у минулому - це процес визначення дати, яка знаходиться в майбутньому або минулому від поточної дати. Програмісти часто використовують це для створення приказних функцій або для розрахунку термінів дії.

Як це зробити:
```java
// Обчислення дати в майбутньому
LocalDate futureDate = LocalDate.now().plusDays(7);

// Обчислення дати в минулому
LocalDate pastDate = LocalDate.now().minusDays(7);

System.out.println(futureDate);
System.out.println(pastDate);

//Виведення результату:
//2021-05-17
//2021-05-03
```

Поглиблене дослідження:
Завдяки розширенню Java 8, програмісти можуть легко обчислювати дати в майбутньому та минулому за допомогою класів LocalDate та LocalTime. Історично, перед Java 8, розрахунок дат був більш складним та вимагав використання сторонніх бібліотек, таких як Joda-Time. Однак, з Java 8, ця функція стала доступною просто за допомогою вбудованих класів.

Додатково:
- Для нестандартних форматів дати, можна використовувати DateTimeFormatter клас.
- Як альтернативу, можна використовувати клас Java Calendar для розрахунку дат, проте даний метод не є настільки зручним та простим як вбудовані класи.
- Для розрахунку на основі географічної розсіїності, можна використовувати Date/Time API Java для задання часових зон.

Дивись також:
- [Документація Java 8 про LocalDate та LocalTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Порівняння класів Date, Calendar та LocalDate/LocalTime](https://www.baeldung.com/java-date-vs-calendar-vs-localdate)
- [Java Date and Time API за прикладами](https://www.mkyong.com/java8/java-8-date-time-api-examples/)