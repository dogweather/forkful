---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Java: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości jest powszechnym zadaniem dla programistów, które polega na wyliczeniu daty w oparciu o daną datę i ustalony odstęp czasu (np. dni, miesięcy, lat). Programiści często wykonują to zadanie w celu wyświetlenia informacji o przyszłych wydarzeniach lub wyliczenia daty wstecz na potrzeby obliczeń.

## Jak to zrobić:
```java
import java.util.Calendar;
import java.util.Date;
import java.text.SimpleDateFormat;

public class DateCalculator {
  public static void main(String[] args) {
    //Obliczanie daty w przyszłości
    Calendar cal = Calendar.getInstance();
    cal.add(Calendar.MONTH, 3);
    Date futureDate = cal.getTime();
    
    //Obliczanie daty w przeszłości
    cal.add(Calendar.YEAR, -2);
    Date pastDate = cal.getTime();
    
    //Formatowanie daty
    SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
    String formattedFuture = format.format(futureDate);
    String formattedPast = format.format(pastDate);
    
    //Wyświetlenie wyniku
    System.out.println("Data za 3 miesiące: " + formattedFuture);
    System.out.println("Data sprzed 2 lat: " + formattedPast);
  }
}
```

Output:
```
Data za 3 miesiące: 20-08-2020
Data sprzed 2 lat: 20-05-2018
```

## Głębszy zanurzenie:
Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki bibliotece Java Calendar, która umożliwia manipulację datą i czasem. Alternatywne rozwiązania to użycie biblioteki Joda-Time lub metody LocalDate klasy LocalDateTime w nowszych wersjach Javy. Implementacja z użyciem klasy Calendar może być nieprecyzyjna, ponieważ ta klasa ma wiele niedociągnięć i lepiej jest korzystać z bardziej aktualnych i zaawansowanych rozwiązań.

## Zobacz też:
- [Dokumentacja Java Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Biblioteka Joda-Time](https://www.joda.org/joda-time/)
- [Dokumentacja Java Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)