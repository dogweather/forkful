---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja daty z ciągu znaków, znana jako analiza składniowa daty, to proces przekształcania tekstu na typ daty lub czasu. Programiści robią to, aby umożliwić programom rozumienie i manipulację danymi daty.

## Jak to zrobić:
Java posiada wbudowaną bibliotekę ```java.time``` do obsługi dat. 

Oto przykład jak przekształcić ciąg znaków na datę:
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");
        String dateInString = "08-08-2022";
        LocalDate localDate = LocalDate.parse(dateInString, formatter);

        System.out.println(localDate); 
    }
}
```
Będziemy mieli taki wynik:
```Java
2022-08-08
```
W tym przypadku przekształcamy datę typu String "08-08-2022" w typ daty LocalDate.

## Głębsza analiza:
- **Historyczny kontekst**: Początkowo, parsowanie daty odbywało się za pomocą klas `Date` i `SimpleDateFormat` w pakiecie `java.util`. Ale od wersji Java 8, poprzez wprowadzenie nowego API daty i czasu, te klasy stały się przestarzałe.
- **Alternatywy**: Można również używać bibliotek zewnętrznych, takich jak Joda-Time, do parsowania dat, które oferują większą elastyczność. Jednak przy korzystaniu z JDK 8 lub nowszej, zaleca się korzystanie z pakietu `java.time`.
- **Szczegóły implementacji**: Używając `java.time.LocalDateTime` można przeprowadzić analizę składniową ciągu znaków do daty, a także czasu, jeśli jest to konieczne. Można to zrobić za pomocą metody `LocalDateTime.parse()`. Dla specyficznych formatów daty, można użyć klasy `DateTimeFormatter`.

## Zobacz też:
1. Dokumentacja API Java Time: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. Poradnik Oracle na temat nowego API daty i czasu: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
3. Dokumentacja API Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)