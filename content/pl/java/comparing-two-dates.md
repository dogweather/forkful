---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównanie dwóch dat, to sprawdzanie, która data jest wcześniejsza lub późniejsza. Programiści robią to, aby manipulować danymi związanymi z czasem.

## Jak to zrobić:

Używamy klasy `java.time.LocalDate`, dostępnej w Java 8 i późniejszych wersjach, do reprezentowania i porównywania dat. Oto przykładowy kod: 

```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {

        // Tworzymy dwie daty
        LocalDate dateA = LocalDate.of(2022, 11, 22);
        LocalDate dateB = LocalDate.of(2022, 11, 23);

        // Porównujemy daty i drukujemy wynik
        if (dateA.isBefore(dateB)) {
            System.out.println(dateA + " jest przed " + dateB);
        } else if (dateA.isAfter(dateB)) {
            System.out.println(dateA + " jest po " + dateB);
        } else {
            System.out.println("Daty są takie same");
        }
    }
}
```

Efekt wywołania kodu:
`2022-11-22 jest przed 2022-11-23`

## Głębokie wgłębienie

1. **Historical context:** W przeszłości, daty porównywano używając klas `java.util.Date` i `java.util.Calendar`. Wprowadzenie `java.time.LocalDate` w Java 8 uprościło te zadania.

2. **Alternatives:** Istnieją alternatywne biblioteki do obsługi czasu, takie jak Joda-Time.

3. **Implementation details:** Kiedy porównujesz dwie daty przy użyciu metody `isBefore` lub `isAfter`, Java wewnętrznie porównuje podane daty na podstawie ich reprezentacji jako dni od epoki (1970-01-01).

## Zobacz również

- Dokumentacja API dla `java.time.LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Poradnik Oracle jak używać `java.time` pakietu: https://docs.oracle.com/javase/tutorial/datetime/iso/index.html
- Wyjaśnienie StackOverflow na temat porównywania dat w Javie: https://stackoverflow.com/questions/7080205/how-to-compare-dates-in-java