---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Java: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Często zdarza się, że w programowaniu musimy przekonwertować datę na string, aby wyświetlić ją w odpowiednim formacie lub wykonać inne operacje na niej. W tym artykule dowiesz się, jak to zrobić w języku Java.

## Jak To Zrobić
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {

    public static void main(String[] args) {
        // tworzymy obiekt LocalDate z datą 25 grudnia 2019
        LocalDate date = LocalDate.of(2019, 12, 25);

        // tworzymy obiekt DateTimeFormatter i określamy pożądany format
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");

        // przekształcamy datę na string przy użyciu wybranego formatu
        String dateString = date.format(formatter);

        // wyświetlamy wynik
        System.out.println(dateString); // 25.12.2019
    }
}
```

W powyższym przykładzie najpierw tworzymy obiekt LocalDate, który przechowuje informację o dacie. Następnie za pomocą obiektu DateTimeFormatter określamy, w jakim formacie chcemy wyświetlić datę. W ostatnim kroku, przy użyciu metody "format", przekształcamy datę na string. W ten sposób możemy łatwo wyświetlić datę w wybranym przez nas formacie.

## Deep Dive
Aby jeszcze lepiej zrozumieć, jak działa konwersja daty na string, przyjrzyjmy się wewnętrznemu mechanizmowi. W języku Java, klasa LocalDate przechowuje informację o dacie w postaci liczby dni od początku ery (1 stycznia 1970 roku). Natomiast formatowanie daty odbywa się poprzez użycie klasy DateTimeFormatter, która wykorzystuje wzorce, aby zmienić liczbę dni na czytelny dla nas string.

## Zobacz Również
- [Java Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Tutorial: Przetwarzanie dat w Java](https://stackoverflow.com/questions/18683658/convert-java-util-date-to-string)