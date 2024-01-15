---
title:                "Обчислення дати у майбутньому або минулому."
html_title:           "Java: Обчислення дати у майбутньому або минулому."
simple_title:         "Обчислення дати у майбутньому або минулому."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дати в майбутньому або минулому може бути корисним для планування подій, знайомства з історією або розв'язання випадків, пов'язаних з часовими рамками.

## Як це зробити

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculator {
    public static void main(String[] args) {
        // встановлення формату дати
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        
        // створення початкової дати
        LocalDate startDate = LocalDate.of(2021, 10, 25);
        
        // додавання 5 днів
        LocalDate futureDate = startDate.plusDays(5);
        
        // вивід результату
        System.out.println("Майбутня дата: " + formatter.format(futureDate));
        
        // віднімання 10 років
        LocalDate pastDate = startDate.minusYears(10);
        
        // вивід результату
        System.out.println("Минула дата: " + formatter.format(pastDate));
    }
}
```

Вивід:
```
Майбутня дата: 30/10/2021
Минула дата: 25/10/2011
```

## Глибока загальудження

Java пропонує різні методи для роботи з датами, такі як `plusDays ()` і `minusYears ()`, які дозволяють додавати або віднімати певну кількість днів, місяців або років до стартової дати. Важливо також враховувати формат дати, щоб отримати зрозумілий вивід результату.

## Дивіться також

- [Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Розрахунок дат на Java](https://www.baeldung.com/java-date-time-manipulation)