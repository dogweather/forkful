---
title:                "Pobieranie bieżącej daty"
html_title:           "Java: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

Dlaczego: Dlaczego warto poznać obecny dzień w języku Java?

Pozyskanie obecnego dnia ma wiele praktycznych zastosowań w programowaniu. Może to być przydatne przy tworzeniu aplikacji bądź skryptów, gdzie potrzebujemy informacji o aktualnej dacie, np. przy tworzeniu systemów rezerwacji lub harmonogramów.

## Jak to zrobić?

Pobranie obecnego dnia nie jest skomplikowane dzięki wbudowanej w język Java klasie "LocalDate". Wystarczy zadeklarować obiekt tej klasy i wykorzystać odpowiednią metodę.

```Java
LocalDate dzisiaj = LocalDate.now(); // Deklarujemy obiekt klasy LocalDate i przypisujemy mu obecną datę
System.out.println(dzisiaj); // Wypisujemy obecną datę
```

Możemy również wyświetlić poszczególne elementy daty, takie jak dzień, miesiąc czy rok, wykorzystując odpowiednie metody klasy LocalDate.

```Java
System.out.println(dzisiaj.getDayOfMonth()); // Wypisujemy obecny dzień miesiąca
System.out.println(dzisiaj.getMonth()); // Wypisujemy obecny miesiąc
System.out.println(dzisiaj.getYear()); // Wypisujemy obecny rok
```

## Deep Dive

W języku Java istnieją różne sposoby na pozyskanie obecnego czasu, takie jak klasa "Date" czy "Calendar". Jednak metoda "LocalDate" jest preferowanym sposobem, ponieważ jest nowocześniejsza i bardziej intuicyjna w użyciu.

Klasa "LocalDate" korzysta z klasy "Clock" do pozyskiwania daty i czasu, dlatego jej wykorzystywanie jest dokładniejsze i bezpieczniejsze niż w przypadku innych metod.

## Zobacz również

- Dokumentacja klasy "LocalDate": https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Przykładowe aplikacje wykorzystujące obecną datę: https://www.baeldung.com/java-current-date