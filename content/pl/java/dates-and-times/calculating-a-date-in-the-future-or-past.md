---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:31:56.071119-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Manipulacja datami to ogólnie zmiana wartości daty na wcześniejszą lub późniejszą. Programiści robią to, by obsługiwać rezerwacje, terminy, przypomnienia czy okresy ważności.

## Jak to zrobić:
W Java korzystamy z `java.time`, zestawu klas wprowadzonych w Java 8.

```Java
import java.time.LocalDate;
import java.time.Period;

public class FuturePastDateCalculation {

    public static void main(String[] args) {
        // Ustal dzisiejszą datę
        LocalDate today = LocalDate.now();
        System.out.println("Dzisiaj: " + today);

        // Dodaj 10 dni do aktualnej daty
        LocalDate tenDaysLater = today.plusDays(10);
        System.out.println("Za 10 dni: " + tenDaysLater);

        // Odejmij 1 miesiąc
        LocalDate oneMonthBefore = today.minusMonths(1);
        System.out.println("Miesiąc temu: " + oneMonthBefore);
    }
}
```
Wyjście:
```
Dzisiaj: 2023-04-01
Za 10 dni: 2023-04-11
Miesiąc temu: 2023-03-01
```

## Głębsze spojrzenie:
Przed Java 8, manipulacja datami była kłopotliwa i błędna. Używanie `java.util.Date` i `SimpleDateFormat` przeplatało się z problemami związanych z bezpieczeństwem wątków i niewygodnym API. `java.time` wprowadza ludzkie podejście do czasu: niezmienne obiekty (`Immutable`), klarowne metody, i unikanie pułapek stref czasowych.

Alternatywy zawierają biblioteki zewnętrzne jak Joda-Time, ale `java.time` jest zalecanym rozwiązaniem od Java 8. W razie potrzeby istnieje też obsługa konwersji pomiędzy starymi a nowymi rodzajami dat przez `.toInstant()` i `.from()`.

W praktyce, wybór techniki zależy od wymagań. Do obsługi złożonych operacji na datach może być potrzebne użycie dodatkowych klas jak `LocalDateTime` czy `ZonedDateTime`.

## Zobacz również:
- [Oracle's Java Date Time API Guide](https://docs.oracle.com/javase/tutorial/datetime/)
- [Java 8 Date Time - Tutorial by Vogella](http://www.vogella.com/tutorials/JavaDateTimeAPI/article.html)
- [Baeldung's Guide to Java 8 Date Time API](https://www.baeldung.com/java-8-date-time-intro)

Warto też zajrzeć do źródła samego JDK, by zobaczyć, jak metody są zaimplementowane – to dobre ćwiczenie na rozumienie kodu.
