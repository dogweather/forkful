---
title:                "Pobieranie aktualnej daty"
html_title:           "Java: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Pobieranie aktualnej daty to podstawowa funkcja w wielu programach. Programiści używają jej w celu uzyskania aktualnego czasu i daty dla działań takich jak tworzenie logów, wyświetlanie dat w aplikacjach lub manipulowanie danymi. W prostych słowach, jest to sposób na uzyskanie informacji o aktualnym czasie w naszym kodzie.

## Jak to zrobić:

```java
import java.time.LocalDate;  // importujemy klasę LocalDate z pakietu java.time

public class AktualnaData {

    public static void main(String[] args) {
        
        // używamy metody statycznej now(), aby utworzyć obiekt klasy LocalDate 
        LocalDate dzis = LocalDate.now();
        
        System.out.println("Dzisiejsza data: " + dzis);
    }
}
```

Output:

```
Dzisiejsza data: 2021-06-30
```

## Głębszy zanurzenie:

Pobieranie aktualnej daty jest możliwe dzięki wprowadzeniu pakietu java.time w Javie 8. Wcześniej, programiści mogli użyć klasy Date lub Calendar, ale wprowadzenie klasy LocalDate ułatwiło operowanie datami i czasem w bardziej czytelny sposób. Istnieją także inne sposoby na pobieranie aktualnej daty, na przykład za pomocą biblioteki Joda-Time lub wykorzystując serwer czasu internetowego (NTP). Wszystkie te rozwiązania mają swoje plusy i minusy, więc ważne jest, aby wybrać najbardziej odpowiednie dla swojego projektu.

## Zobacz także:

- [Dokumentacja Java 8: Klasa LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial Java Podstawy: Pobieranie aktualnego czasu i daty](https://javastart.pl/kurs/java/java-podstawy/aktualny-czas-i-data)
- [Oracle Java Tutorial: Pobieranie aktualnego czasu i daty z NTP](https://docs.oracle.com/en/java/javase/11/core/date-time-services.html#GUID-48E72DBE-D890-4E35-8F3E-EC164DADC0B5)