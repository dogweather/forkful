---
title:                "Java: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami potrzebujemy obliczyć datę w przyszłości lub w przeszłości, na przykład kiedy chcemy wyświetlić datę po upływie określonego czasu lub aby ustawić przypomnienie na konkretny dzień. W tym artykule dowiesz się, jak to zrobić za pomocą języka Java.

## Jak to zrobić

Obliczenie daty w przyszłości lub przeszłości w języku Java jest proste i można to zrobić na kilka sposobów. Jednym z nich jest użycie klasy `Calendar` i jej metody `add()`. Przyjmijmy, że chcemy dodać dwa lata do bieżącej daty i wyświetlić nową datę w formacie `dd.MM.yyyy`. Poniżej znajduje się przykładowy kod do tego zadania:

```Java
Calendar calendar = Calendar.getInstance(); // Utworzenie obiektu klasy Calendar z bieżącą datą
calendar.add(Calendar.YEAR, 2); // Dodanie dwóch lat do bieżącej daty
SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy"); // Utworzenie obiektu klasy SimpleDateFormat z żądanym formatem daty
System.out.println(sdf.format(calendar.getTime())); // Wyświetlenie obliczonej daty
```

Na wyjściu otrzymujemy "29.03.2022".

Innym sposobem jest użycie klasy `LocalDate` z pakietu `java.time`, która pojawiła się w Javie 8. Poniżej znajduje się przykładowy kod, który oblicza datę za 1000 dni:

```Java
LocalDate today = LocalDate.now(); // Utworzenie obiektu klasy LocalDate z bieżącą datą
LocalDate futureDate = today.plusDays(1000); // Obliczenie daty za 1000 dni
DateTimeFormatter dtf = DateTimeFormatter.ofPattern("dd.MM.yyyy"); // Utworzenie obiektu klasy DateTimeFormatter z żądanym formatem daty
System.out.println(dtf.format(futureDate)); // Wyświetlenie obliczonej daty
```

Na wyjściu otrzymujemy "22.12.2021".

## Deep Dive

W pierwszym przykładzie użyliśmy klasy `Calendar`, która jest dostępna w Javie od wersji 1.1, ale nie jest zalecana do używania przez twórców Javy. Zamiast tego zalecane jest używanie klasy `LocalDate` z pakietu `java.time`, która jest bardziej intuicyjna i bardziej odporna na błędy.

Klasy `Calendar` i `LocalDate` posiadają wiele innych metod, które można wykorzystać do obliczania dat w przyszłości lub przeszłości. Należy pamiętać, że daty w obu tych klasach są niemodyfikowalne, co oznacza, że każde wykonanie jakiejś operacji na dacie zwraca nową datę, a nie modyfikuje istniejącą.

## Zobacz też

- [Oficjalna dokumentacja klasy Calendar w Javie](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Oficjalna dokumentacja klasy LocalDate w Javie](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Poradnik o pakiecie java.time na stronie Baeldung](https://www.baeldung.com/java-8-date-time-intro)
- [Poradnik o formatowaniu dat w Javie na stronie Mkyong](https://mkyong.com/java/java-how-to-get-current-date-time-date-and-calender/)