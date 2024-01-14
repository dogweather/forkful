---
title:                "Java: Pobieranie aktualnej daty"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach istnieje wiele powodów, dla których ktoś chciałby uzyskać aktualną datę. Może być to potrzebne do śledzenia czasu wykonywania pewnych operacji w programach, do wyświetlania aktualnej daty w interfejsie użytkownika lub do wyliczenia wieku osoby na podstawie jej daty urodzenia. Dzięki otrzymaniu aktualnej daty, można również sprawdzić, czy się zgadza z datami w plikach lub bazach danych. 

## Jak to zrobić

Aby uzyskać aktualną datę w języku Java, należy użyć klasy `LocalDate` z biblioteki `java.time`. Możemy to zrobić za pomocą dwóch prostych linijek kodu:

```java
import java.time.LocalDate;

LocalDate currentDate = LocalDate.now();
```

Pierwsza linijka importuje klasę `LocalDate`, a druga tworzy nowy obiekt `LocalDate` o nazwie `currentDate` i przypisuje mu aktualną datę. Możemy również użyć metody `now()` bezpośrednio bez tworzenia obiektu, ale wtedy nie będziemy mieć możliwości odwołania się do daty w późniejszej części programu.

Możemy również uzyskać aktualny czas wraz z datą przez użycie klasy `LocalDateTime` i metody `now()` z klasy `LocalDateTime`.

```java
import java.time.LocalDateTime;

LocalDateTime currentDateTime = LocalDateTime.now();
```

W obu przypadkach, jeśli uruchomimy nasz kod, otrzymamy aktualną datę i czas w formacie `yyyy-MM-dd` (na przykład `2020-11-05`) lub `yyyy-MM-dd-HH-mm-ss` (na przykład `2020-11-05-14-30-10`).

## Deep Dive 

Klasa `LocalDate` i `LocalDateTime` z biblioteki `java.time` zostały wprowadzone w Java 8 jako część pakietu `java.time` wraz z wieloma innymi klasami, takimi jak `LocalTime` czy `ZonedDateTime`. Te klasy zapewniają nowoczesne i elastyczne podejście do obsługi dat i czasu w języku Java.

Klasy te opierają się na koncepcji czasowej FourTen (opracowanej przez Stephena Colebourne'a) i umożliwiają obsługę stref czasowych, w tym uwzględnienie zmiany czasu, zmian czasowych w różnych krajach oraz różnych metody obliczania wieku na podstawie daty urodzenia.

Klasy `LocalDate` i `LocalDateTime` są również całkowicie niezmiennicze (ang. immutable), co oznacza, że nie mogą zostać zmienione po utworzeniu. To ważne w kontekście wielowątkowości, ponieważ zapewnia to bezpieczeństwo w przypadku jednoczesnego dostępu do tych obiektów.

## Zobacz również

- [Java 8 Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [LocalDate documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [SimpleDateFormat vs DateTimeFormatter](https://joda.org/joda-time/apidocs/org/joda/time/format/DateTimeFormat.html)