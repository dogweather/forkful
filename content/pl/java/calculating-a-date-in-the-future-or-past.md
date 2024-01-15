---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "Java: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego

Chcesz wiedzieć, jak obliczyć datę w przyszłości lub przeszłości w Javie? Być może musisz wykonać obliczenia dla swojej aplikacji lub projektu. W tym artykule dowiesz się, jak prostym kodem w Javie wykonać tę operację.

# Jak To Zrobić

Do obliczenia daty w przyszłości lub przeszłości w Javie potrzebne będą nam dwie rzeczy: obiekt typu LocalDate oraz obiekt typu Period.

Pierwszym krokiem jest stworzenie obiektu typu LocalDate z aktualną datą. Możemy to zrobić za pomocą metody `now()`.

```Java
LocalDate dzisiaj = LocalDate.now();
```

Następnie, aby obliczyć datę w przyszłości, możemy wykorzystać metodę `plus()` na obiekcie LocalDate i przekazać jako argument obiekt typu Period z określoną ilością dni, miesięcy lub lat, które chcemy dodać.

```Java
LocalDate dataWPrzyszlosci = dzisiaj.plus(Period.ofDays(7));
```

W ten sam sposób, aby obliczyć datę w przeszłości, możemy użyć metody `minus()` i przekazać obiekt Period ze zwiększoną wartością ujemną.

```Java
LocalDate dataWPrzeszlosci = dzisiaj.minus(Period.ofMonths(2));
```

Po wykonaniu tych operacji możemy wyświetlić obliczone daty za pomocą metody `toString()`.

```Java
System.out.println(dataWPrzyszlosci.toString()); //wyświetli datę z siedmioma dniami w przód
```

Możesz także dodać lub odjąć więcej niż tylko dni lub miesięcy. Możesz także przekazać jako argumenty lata, miesiące i dni osobno.

```Java
LocalDate dataWInnejJednostce = dzisiaj.plus(Period.of(2, 3, 10)); //doda 2 lata, 3 miesiące i 10 dni
```

# Deep Dive

Metoda `plus()` i `minus()` używają obiektu typu TemporalAmount, który jest interfejsem rozszerzającym interfejs TemporalUnit. Dzięki temu możemy przekazać różne obiekty jako argumenty, takie jak Period, Duration czy Instant.

W przypadku użycia obiektu typu Duration, możemy dodać lub odjąć liczbę sekund, minut, godzin czy nawet dni.

```Java
LocalDate data = dzisiaj.plus(Duration.ofMinutes(30)); //dodaje 30 minut do aktualnej daty
```

Możemy również przekazać ilość dni jako argument do metody `plusDays()` lub `minusDays()`.

```Java
LocalDate dataZNastepnegoTygodnia = dzisiaj.plusDays(7); //dodaje 7 dni do aktualnej daty
```

# Zobacz również

- [Java 8 LocalDate Class: A Brief Introduction](https://www.baeldung.com/java-8-localdate)
- [Java LocalDate API Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Period API Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)