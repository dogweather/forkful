---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:15:03.515027-07:00
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pobieranie aktualnej daty to po prostu uchwycenie bieżącego dnia, miesiąca i roku. Programiści robią to, by śledzić czas, logować zdarzenia czy wyświetlać aktualizacje w czasie rzeczywistym.

## Jak to zrobić:
```java
import java.time.LocalDate;

public class GetCurrentDate {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Dzisiaj jest: " + today);
    }
}
```
Wyjście:
```
Dzisiaj jest: 2023-04-02
```

## Szczegółowe informacje:
Początkowo, w Javie datę i czas obsługiwała klasa `java.util.Date`, ale była niewygodna i zbyt podatna na błędy. Java 8 wprowadziła nowe API do obsługi czasu – `java.time`, które jest bardziej intuicyjne i bezpieczne. Pozwala na łatwe pobieranie aktualnej daty, tak jak w przykładzie powyżej, przy użyciu klasy `LocalDate`. Jeśli potrzebujesz większej precyzji, możesz użyć `LocalTime` dla czasu lub `LocalDateTime` dla daty i czasu razem. Ważne jest zrozumienie, że `LocalDate.now()` korzysta z defaultowej strefy czasowej.

## Zobacz również:
- [LocalDate dokumentacja Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Data i Czas w Javie 8 - Tutorial](https://www.baeldung.com/java-8-date-time-intro)
