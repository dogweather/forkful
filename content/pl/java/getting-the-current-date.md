---
title:    "Java: Uzyskiwanie aktualnej daty"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobranie aktualnej daty jest jedną z podstawowych czynności w wielu programach Java. Wiele aplikacji wymaga dostępu do aktualnego czasu, aby wykonywać różne operacje na danych. Bez aktualnej daty moglibyśmy mieć trudności w śledzeniu wydarzeń i wykonywaniu określonych zadań. W artykule tym przyjrzymy się temu, jak przy pomocy prostych kroków możemy uzyskać aktualną datę w Javie.

## Jak To Zrobić

Aby pobrać aktualną datę w Javie, wykorzystamy klasę `LocalDate` z pakietu `java.time`. Oto przykładowy kod, który wyświetli bieżącą datę w formacie `yyyy-MM-dd`:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class AktualnaData {
    public static void main(String[] args) {
        // pobranie aktualnej daty
        LocalDate data = LocalDate.now();
        // użycie formattera do zmiany formatu na yyyy-MM-dd
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        // wyświetlenie daty w określonym formacie
        System.out.println("Aktualna data: " + formatter.format(data));
    }
}
```

Po uruchomieniu programu powinniśmy zobaczyć następujący wynik:

```
Aktualna data: 2021-06-14
```

Możemy także utworzyć własny wybrany format daty, korzystając z innych liter formatujących, takich jak `d` dla dnia, `M` dla miesiąca czy `y` dla roku. Wybór zależy od naszych potrzeb i wymagań.

## Głębsza Analiza

Klasa `LocalDate` posiada wiele przydatnych metod do manipulowania datami, takich jak `plusDays()`, `minusMonths()` czy `withYear()`. Możemy także użyć innych klas, takich jak `LocalDateTime` czy `ZonedDateTime`, aby uzyskać bieżącą datę w określonej strefie czasowej.

Klasa `DateTimeFormatter` pozwala nam na jeszcze większą kontrolę nad formatowaniem daty, na przykład dodając nazwę miesiąca lub dnia tygodnia.

Możemy również pobrać aktualny czas lub strefę czasową używając odpowiednich klas, takich jak `LocalTime` czy `ZoneId`.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o pobieraniu daty w Javie, polecamy Ci zapoznanie się z następującymi materiałami:

- Dokumentacja klasy `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Poradnik o formatowaniu daty w Javie: https://www.baeldung.com/java-string-to-date
- Opis klas do obsługi daty i czasu w Javie: https://www.tutorialspoint.com/java8/java8_date_time_api.htm