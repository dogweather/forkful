---
title:                "Java: Porównywanie dwóch dat"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest kluczowym aspectem wielu aplikacji Java. Może być wykorzystane w celu porównania dat bieżących z datami w przeszłości lub przyszłości, określenia różnicy czasu lub ustalenia, który z dwóch wydarzeń miało miejsce wcześniej. Jest to przydatne w wielu przypadkach, szczególnie w systemach rezerwacji, zarządzaniu projektami lub różnego rodzaju harmonogramach.

## Jak to zrobić

Aby porównać dwie daty w języku Java, należy użyć metody `compareTo()` klasy `java.util.Date`. Należy pamiętać, że ta metoda porównuje daty z uwzględnieniem czasu, dlatego może zwrócić wartość różną od zera, nawet jeśli daty są identyczne, ale zawierają różne wloty czasowe.

Przykład kodu:

```java
import java.util.Date;

public class DateComparison {
    public static void main(String[] args) {
        // tworzenie dwóch dat
        Date date1 = new Date(121, 6, 4); // 4 lipca 2021
        Date date2 = new Date(120, 6, 4); // 4 lipca 2020
        
        // porównywanie dat
        int result = date1.compareTo(date2);
        
        // wypisanie wyniku
        if (result > 0) {
            System.out.println("Data1 jest późniejsza od daty2.");
        } else if (result == 0) {
            System.out.println("Obie daty są identyczne.");
        } else {
            System.out.println("Data2 jest późniejsza od daty1.");
        }
    }
}
```

Przykładowy wynik:

```
Data1 jest późniejsza od daty2.
```

## Głębsze zagadnienia

Warto pamiętać, że w Javie istnieją również klasy `LocalDate`, `LocalDateTime` i `LocalTime`, które umożliwiają porównywanie dat bez uwzględniania czasu oraz obsługę dat i czasów w różnych strefach czasowych.

Należy również zwrócić uwagę na możliwość użycia metody `equals()` do porównywania dat w Javie. Metoda ta porównuje daty bez uwzględniania czasu, więc jest to przydatne, jeśli interesuje nas porównanie samej daty, a nie uwzględnienie czasu.

## Zobacz również

- [Dokumentacja klasy Date w języku Java](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Porównywanie dat w języku Java - przykłady](https://www.tutorialspoint.com/java/util/date_compareto.htm)
- [Jak porównywać daty w Javie](https://www.baeldung.com/java-date-compare)