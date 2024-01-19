---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Java: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Обчислення дати в майбутньому або минулому - це спосіб програмування, щоб дізнатися конкретний день, що відбувається на проміжок часу до або після даної дати. Програмісти роблять це для створення різного funcionalu, наприклад, відстеження термінів сплати, нагадування про події тощо.

##  Як це робити:

```Java
import java.time.LocalDate;
import java.time.Period;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        Period p = Period.ofDays(7); // вказуємо, скільки днів хочемо додати
        LocalDate futureDate = today.plus(p);
        
        System.out.println("Today's Date: " + today);
        System.out.println("Date after 7 days: " + futureDate);
    }
}
```
 
Вихідний код:

```Java
Today's Date: 2022-02-01
Date after 7 days: 2022-02-08
```


## Поглиблення

### Історичний контекст
Людству завжди потрібно було розраховувати дати в минулому та майбутньому, ще з часів коли були створені перші календарі. Для комп’ютерів це стало можливим з появою перших мов програмування. Проте, з часом виявилося, що собівартість таких обчислень може бути доволі високою через складність алгоритмів та неефективність реалізацій.

### Альтернативи
Java пропонує кілька альтернатив для роботи з датами. Ми можемо використовувати `Calendar`, `Date`, `Time`, `YearMonthDay` та інші класи. Ми також можемо користуватися сторонніми бібліотеками, такими як `Joda-Time`.

### Реалізація
В Java 8 введено новий API для роботи з датою і часом, який значно полегшує обчислення дати в майбутньому або в минулому. Ми використовуємо клас `LocalDate` і методи `now()` для отримання поточної дати та `plus()` для додавання періоду до поточної дати.

## Дивись також

1. [`LocalDate`](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html) - Документація Oracle для `LocalDate`.
2. [`Period`](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html) - Документація Oracle для `Period`.
3. [Основи DateTime API](https://docs.oracle.com/javase/tutorial/datetime/iso/overview.html) - Вступ до нового API DateTime в Java 8.
4. [Joda-Time](http://www.joda.org/joda-time/) - Бібліотека Joda-Time для роботи з датами та часом в Java.