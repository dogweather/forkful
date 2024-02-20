---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:52.986340-07:00
description: "Pobranie bie\u017C\u0105cej daty w Java to podstawowa operacja, kt\xF3\
  ra pozwala programistom manipulowa\u0107 obiektami dat do operacji takich jak logowanie,\
  \ obliczenia\u2026"
lastmod: 2024-02-19 22:04:54.417792
model: gpt-4-0125-preview
summary: "Pobranie bie\u017C\u0105cej daty w Java to podstawowa operacja, kt\xF3ra\
  \ pozwala programistom manipulowa\u0107 obiektami dat do operacji takich jak logowanie,\
  \ obliczenia\u2026"
title: Pobieranie aktualnej daty
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobranie bieżącej daty w Java to podstawowa operacja, która pozwala programistom manipulować obiektami dat do operacji takich jak logowanie, obliczenia dat i warunki oparte na czasie. Jest to niezbędne w aplikacjach, gdzie kluczowe są śledzenie, planowanie i analiza danych czasowych.

## Jak:
Java oferuje wiele sposobów na pobranie bieżącej daty, korzystając zarówno ze starej klasy `java.util.Date`, jak i nowszego pakietu `java.time` (wprowadzonego w Java 8), który jest bardziej wszechstronny i intuicyjny.

### Używając `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Przykład wyjścia: 2023-04-01
    }
}
```
### Używając `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // Przykład wyjścia: 2023-04-01T12:34:56.789
    }
}
```
### Używając `java.util.Date` (Starsze)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // Przykład wyjścia: Sob Apr 01 12:34:56 BST 2023
    }
}
```
### Korzystając z biblioteki zewnętrznej: Joda-Time
Przed Java 8, Joda-Time był de facto standardem dla daty i czasu w Java. Jeśli pracujesz nad starszymi systemami lub preferujesz Joda-Time, oto jak możesz go użyć, aby uzyskać bieżącą datę:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // Przykład wyjścia: 2023-04-01
    }
}
```
**Uwaga:** Mimo że `java.util.Date` i Joda-Time są nadal używane, zaleca się używanie pakietu `java.time` w nowych projektach ze względu na jego niezmienność i kompleksowe API do obsługi dat i czasów.
