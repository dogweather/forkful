---
title:    "Java: Obliczanie daty w przyszłości lub przeszłości."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy pracować z datami, zarówno przeszłymi jak i przyszłymi. Może to wynikać z potrzeby wyświetlenia odpowiedniej informacji dla użytkownika lub obliczenia rozróżnienia między dwoma datami. W takich sytuacjach konieczne jest umiejętne obliczanie dat w przyszłości lub w przeszłości.

## Jak

Aby wykonać obliczenia daty w przyszłości lub w przeszłości w języku Java, musimy użyć klasy `Calendar`. Pierwszym krokiem będzie utworzenie obiektu `Calendar` i ustawienie aktualnej daty. Następnie, aby obliczyć datę w przyszłości, musimy użyć metody `add()`, która dodaje określoną liczbę dni, miesięcy lub lat do aktualnej daty. Przykładowy kod wyglądałby następująco:

```java
// utworzenie obiektu Calendar
Calendar now = Calendar.getInstance();
// ustawienie aktualnej daty
now.setTime(new Date());
// dodanie 7 dni do aktualnej daty
now.add(Calendar.DATE, 7);
// wyświetlenie obliczonej daty
System.out.println(now.getTime());
```

W powyższym przykładzie dodajemy 7 dni do aktualnej daty, ale możemy również dodać miesiące lub lata, zmieniając odpowiednio drugi argument metody `add()`.

Aby obliczyć datę w przeszłości, możemy użyć metody `roll()`. Różnica między `add()` a `roll()` polega na tym, że `add()` zmienia również bieżący miesiąc lub rok, podczas gdy `roll()` pozostawia je bez zmian. Przykładowy kod obliczający datę w przeszłości wyglądałby następująco:

```java
// utworzenie obiektu Calendar
Calendar now = Calendar.getInstance();
// ustawienie aktualnej daty
now.setTime(new Date());
// przesunięcie daty o 7 dni w przeszłości
now.roll(Calendar.DATE, -7);
// wyświetlenie obliczonej daty
System.out.println(now.getTime());
```

## Deep Dive

Klasa `Calendar` zawiera wiele innych przydatnych metod, które pozwalają na bardziej precyzyjne obliczenia daty, takie jak porównywanie dat lub obliczanie różnicy między dwoma datami. Warto również zauważyć, że klasa `Calendar` jest często używana wraz z klasą `DateFormat`, która umożliwia formatowanie daty w różnych formatach, np. jako tekst czy data.

Duża część funkcjonalności związanych z datami została również dodana do języka Java 8 w postaci klasy `LocalDate`, która oferuje bardziej wygodne metody do obliczania dat w przyszłości lub przeszłości.

## Zobacz także

- [Przewodnik po klasie Calendar w języku Java](https://www.baeldung.com/java-calendar)
- [Dokumentacja klasy Calendar w języku Java] (https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Przewodnik po formacie daty w języku Java] (https://www.baeldung.com/java-date-format)
- [Dokumentacja klasy LocalDate w języku Java] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)