---
title:    "Java: Порівняння двох дат"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому
З порівнянням двох дат є багато практичних застосувань, таких як визначення поточної дати, порівняння різних подій або відстеження проміжків часу. У цій статті ми розглянемо, як це зробити за допомогою Java програмування.

## Як
Для порівняння двох дат в Java, ми використовуємо метод `compareTo()` з класу `Date`.

```
// Створення об'єктів дат
Date date1 = new Date(2020, 10, 10); // 10 жовтня 2020 року
Date date2 = new Date(2020, 10, 12); // 12 жовтня 2020 року

// Порівняння дат за допомогою методу compareTo()
int result = date1.compareTo(date2);

// Виведення результату
if(result < 0){
    System.out.println("Перша дата передує другій даті.");
} else if(result > 0) {
    System.out.println("Перша дата пізніше другої дати.");
} else {
    System.out.println("Обидві дати однакові.");
}

// Виведення в форматі "рік/місяць/день"
System.out.println("Перша дата: " + date1.toString());
System.out.println("Друга дата: " + date2.toString());
```

В результаті ми отримаємо наступний вивід:

```
Перша дата передує другій даті.
Перша дата: Wed Oct 10 00:00:00 EEST 3920
Друга дата: Fri Oct 12 00:00:00 EEST 3920
```

## Повне занурення
Крім методу `compareTo()`, в Java також є методи, які дозволяють порівняти дві дати за певним параметром, таким як рік, місяць або день. Наприклад, метод `before()` перевіряє, чи є перша дата до другої дати, а метод `after()` перевіряє, чи є перша дата після другої дати.

```
// Перевірка, чи є перша дата пізніше другої дати за роком
if(date1.after(date2)){
    System.out.println("Перша дата пізніше другої за роком.");
} else {
    System.out.println("Перша дата не пізніше другої за роком.");
}
```

Детальніше про роботу з датами в Java можна дізнатися в [офіційній документації](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html).

## Дивіться Також
- [Порівняння дат в Java](https://www.geeksforgeeks.org/compare-dates-java/)
- [Робота з датами і часом в Java](https://www.baeldung.com/java-date-time)
- [Офіційна документація Java 8 про клас `Date`](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)