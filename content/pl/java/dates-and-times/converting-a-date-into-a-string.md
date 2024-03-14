---
date: 2024-01-20 17:36:57.556542-07:00
description: "(Kiedy i dlaczego?) Przekszta\u0142canie daty na \u0142a\u0144cuch znak\xF3\
  w to proces formatowania obiektu daty w spos\xF3b czytelny dla cz\u0142owieka. Programi\u015B\
  ci to robi\u0105, aby\u2026"
lastmod: '2024-03-13T22:44:35.289208-06:00'
model: gpt-4-1106-preview
summary: "(Kiedy i dlaczego?) Przekszta\u0142canie daty na \u0142a\u0144cuch znak\xF3\
  w to proces formatowania obiektu daty w spos\xF3b czytelny dla cz\u0142owieka. Programi\u015B\
  ci to robi\u0105, aby\u2026"
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
---

{{< edit_this_page >}}

## What & Why?
(Kiedy i dlaczego?)
Przekształcanie daty na łańcuch znaków to proces formatowania obiektu daty w sposób czytelny dla człowieka. Programiści to robią, aby wyświetlać daty w zrozumiałych formatach lub zapisywać je w bazach danych i plikach tekstowych.

## How to:
(Jak to zrobić?)
```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatter {
    public static void main(String[] args) {
        // Tworzenie obiektu daty
        Date currentDate = new Date();
        
        // Formatowanie daty na łańcuch znaków
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        String dateString = formatter.format(currentDate);
        
        // Wyświetlanie sformatowanej daty
        System.out.println(dateString);
    }
}
```
Wyjście:
```
31-03-2023 15:45:12
```

## Deep Dive:
(Zagłębiając się)
Przekształcanie dat na łańcuchy znaków jest konieczne od początków programowania. W Javie, używaliśmy `SimpleDateFormat` od Javy 1.1. Z czasem doszły nowsze klasy jak `DateTimeFormatter` z Java 8, które oferują większą elastyczność i bezpieczeństwo wątków. Alternatywą jest również stosowanie zewnętrznych bibliotek jak Joda-Time, które zostały zastąpione przez klasy z pakietu `java.time.*`. Ważne, by pamiętać o odpowiednim doborze formatu: `dd-MM-yyyy` dla dni, miesięcy i lat, `HH:mm:ss` dla godzin, minut i sekund, przy czym wielkie 'HH' oznacza format 24-godzinny.

## See Also:
(Zobacz także)
- Dokumentacja klasy SimpleDateFormat: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
- Dokumentacja Java Date and Time API: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Przewodnik po Java 8 Date Time API: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
- Tutorial do Joda-Time: http://www.joda.org/joda-time/quickstart.html
