---
date: 2024-01-20 17:33:14.811439-07:00
description: "Jak to zrobi\u0107? Wynik."
lastmod: '2024-04-05T21:53:36.728837-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## Jak to zrobić?
```java
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, Month.MARCH, 10);
        LocalDate date2 = LocalDate.of(2023, Month.MARCH, 15);
        
        // Porównanie dat
        if (date1.isBefore(date2)) {
            System.out.println("Data1 jest wcześniejsza niż Data2.");
        } else if (date1.isAfter(date2)) {
            System.out.println("Data1 jest późniejsza niż Data2.");
        } else {
            System.out.println("Data1 i Data2 są tej samej daty.");
        }
        
        // Porównanie z dokładnością do sekundy
        LocalDateTime dateTime1 = LocalDateTime.of(2023, Month.MARCH, 10, 15, 30);
        LocalDateTime dateTime2 = LocalDateTime.of(2023, Month.MARCH, 10, 15, 45);
        
        boolean isAfter = dateTime1.isAfter(dateTime2);
        System.out.println("Czy dateTime1 jest późniejsza niż dateTime2? " + isAfter);
    }
}
```

Wynik:
```
Data1 jest wcześniejsza niż Data2.
Czy dateTime1 jest późniejsza niż dateTime2? false
```

## Deep Dive
Porównywanie dat sięga początków informatyki. W Javie do Java 8 korzystano głównie z `java.util.Date` i `java.util.Calendar`, ale były skomplikowane w użyciu i niewystarczająco dokładne. Teraz `java.time` (z którym pracujemy powyżej) jest standardem, szeroko akceptowanym ze względu na przejrzystość i funkcjonalność.

Inne sposoby to porównywanie timestampów (w milisekundach) lub wykorzystanie bibliotek zewnętrznych, jak Joda-Time (choć od Java 8 mniej popularnych).

Detal implementacyjny: `LocalDate` i `LocalDateTime` są niemutowalne i wątkowo bezpieczne, co oznacza, że możesz używać ich bez obaw o spójność danych w aplikacjach wielowątkowych.

## Zobacz również
- Dokumentacja klasy `LocalDate`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html
- Dokumentacja klasy `LocalDateTime`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDateTime.html
- Porównanie `java.time` z Joda-Time: https://www.baeldung.com/joda-time
- Poradnik Oracle dla `java.time`: https://docs.oracle.com/javase/tutorial/datetime/
