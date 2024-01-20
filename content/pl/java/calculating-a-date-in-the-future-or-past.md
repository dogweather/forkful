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

## Co i Dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu dni od bieżącej daty. Programiści robią to, aby manipulować danymi daty, które są niezbędne w wielu rodzajach aplikacji, takich jak harmonogramy, przypomnienia, czy systemy rezerwacji.

## Jak to zrobić:

Przykładowo, obliczymy datę dla dnia 10 dni od teraz:

```Java
    import java.time.LocalDate;

    public class Main {
        public static void main(String[] args) {
            LocalDate today = LocalDate.now();
            LocalDate futureDate = today.plusDays(10);
            System.out.println("Data za 10 dni to: " + futureDate);
        }
    }
```

W wyniku otrzymamy coś takiego:

```
Data za 10 dni to: 2023-05-10
```

## Głębsze zrozumienie

Metoda `plusDays()` (i jej odpowiednik `minusDays()`) jest częścią biblioteki Java Time dodanej w Java 8. Przed tą wersją, musieliśmy korzystać z `java.util.Calendar`, którego użycie było mniej intuicyjne i bardziej skomplikowane. Alternatywą dla tych metod jest użycie `java.util.GregorianCalendar`. Co do szczegółów implementacji `plusDays()`, to dodaje po prostu odpowiednią liczbę dni do wartości dnia w obiekcie LocalDate, kontrolując jednocześnie przekroczenie miesięcy i lat.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej, sprawdź te źródła:

1. Dokumentacja Oracle na temat klasy [LocalDate](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/time/LocalDate.html).
2. Wprowadzenie do nowego API czasu Java na [Baeldung](https://www.baeldung.com/java-8-date-time-intro).
3. "Java SE 8: Nowe funkcje, aktualizacje i ulepszenia API" od [Oracle](https://www.oracle.com/java/technologies/javase/8-whats-new.html).