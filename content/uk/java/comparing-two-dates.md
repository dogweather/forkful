---
title:                "Java: Порівняння двох дат"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому
У цій Java блозі я розповім вам про те, як порівнювати дві дати в програмуванні. Ця важлива тема допоможе вам в роботі з датами та розрахунках часу.

## Як це зробити
```java
import java.time.LocalDate; // імпортуємо потрібний пакет

public class DateComparison {
    public static void main(String[] args){
        // створюємо дві дати для порівняння
        LocalDate date1 = LocalDate.of(2020, 12, 15);
        LocalDate date2 = LocalDate.of(2021, 1, 1);

        // порівнюємо дві дати
        if(date1.compareTo(date2) > 0){
            System.out.println(date1 + " пізніше за " + date2);
        } else if(date1.compareTo(date2) < 0){
            System.out.println(date1 + " раніше за " + date2);
        } else{
            System.out.println("Дати однакові");
        }
    }
}
```

Приклад виводу:
```
2020-12-15 раніше за 2021-01-01
```

## Глибока аналітика
При порівнянні двох дат в Java використовується метод `compareTo()`, який повертає ціле число, що вказує на відношення між двома датами. Якщо результат більше 0, то перша дата пізніше за другу, якщо менше 0 - раніше, і якщо рівне 0 - дати однакові. Зверніть увагу, що дата у форматі `LocalDate` в Java не містить інформацію про час.

## Дивитися також
- [Офіційна документація Java про роботу з датами](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- [Порівняння дат в інших мовах програмування](https://www.baeldung.com/java-compare-dates)
- [Приклади роботи з датами в Java](https://www.geeksforgeeks.org/java-8-localdate-class/)