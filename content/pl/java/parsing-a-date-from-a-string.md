---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:36:52.987498-07:00
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsowanie daty z tekstu to zamiana reprezentacji tekstowej daty na obiekt daty. Programiści to robią, by mogły one być łatwo obrabiane i porównywane, zgodnie z logicznymi i matematycznymi regułami języków programowania.

## How to: (Jak to zrobić:)
Poniżej znajdziesz prosty kod Java, który demonstruje jak sparsować datę z tekstu:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateText = "2023-04-01";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd", Locale.ENGLISH);
        LocalDate parsedDate = LocalDate.parse(dateText, formatter);
        
        System.out.println("Parsed date is: " + parsedDate);
    }
}
```

Przykładowe wyjście:
```
Parsed date is: 2023-04-01
```

## Deep Dive (Dogłębna analiza)
Początkowo, w Javie data była parsowana głównie za pomocą `SimpleDateFormat` z pakietu `java.util`. Jednak klasa ta nie była bezpieczna przy użyciu w wielowątkowych scenariuszach i dlatego z czasem pojawiła się nowa API - `java.time` (wprowadzona w Java 8), z której pochodzi użyta wyżej klasa `LocalDate`.

Alternatywą jest klasa `Date` ale jest ona uznawana obecnie za przestarzałą (deprecated). Warto też wspomnieć o bibliotece Joda-Time – była popularna przed Java 8, ale od tego czasu rekomendowane jest używanie `java.time`.

Pamiętaj, parsowanie jest wrażliwe na format – musisz znać format źródłowej daty tekstowej, by poprawnie stworzyć wzorzec w `DateTimeFormatter`. Pominięcie lub błędy w formacie mogą prowadzić do wyjątków `DateTimeParseException`.

## See Also (Zobacz również)
- [Dokumentacja klasy LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Dokumentacja klasy DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Przewodnik Oracle po API java.time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Biblioteka Joda-Time](https://www.joda.org/joda-time/)
