---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:23.433070-07:00
description: "Parsowanie daty z ci\u0105gu polega na przekszta\u0142ceniu tekstowej\
  \ reprezentacji daty i czasu na obiekt `Date` lub nowocze\u015Bniejszy obiekt `LocalDateTime`.\u2026"
lastmod: '2024-03-11T00:14:08.464953-06:00'
model: gpt-4-0125-preview
summary: "Parsowanie daty z ci\u0105gu polega na przekszta\u0142ceniu tekstowej reprezentacji\
  \ daty i czasu na obiekt `Date` lub nowocze\u015Bniejszy obiekt `LocalDateTime`.\u2026"
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu polega na przekształceniu tekstowej reprezentacji daty i czasu na obiekt `Date` lub nowocześniejszy obiekt `LocalDateTime`. Programiści robią to, aby manipulować, formatować, porównywać lub przechowywać daty w ustandaryzowanym formacie, co jest kluczowe dla aplikacji wymagających obliczeń datowych, walidacji lub spójnej internacjonalizacji.

## Jak:

### Używając pakietu `java.time` (Polecane w Java 8 i późniejszych):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Wynik: 2023-04-30
    }
}
```

### Używając `SimpleDateFormat` (Starsze podejście):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // Format wyjściowy zależy od domyślnego formatu systemu
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### Używając bibliotek innych firm (np. Joda-Time):
Joda-Time było znaczącą biblioteką stron trzecich, ale teraz jest w trybie konserwacji ze względu na wprowadzenie pakietu `java.time` w Java 8. Jednakże dla tych, którzy używają wersji Java przed 8, Joda-Time jest dobrym wyborem.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // Wynik: 2023-04-30
    }
}
```
Zauważ, że pracując z datami, zawsze należy być świadomym ustawień strefy czasowej, jeśli parsujesz lub formatujesz daty i czasy, a nie tylko daty.
