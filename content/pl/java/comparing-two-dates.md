---
title:                "Java: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym wpisie dokonamy głębokiego zanurzenia w porównywaniu dwóch dat w języku Java. Zobaczymy, dlaczego jest to ważne i jak można to osiągnąć. Włączcie swoje komputery i przygotujcie się do programowania!

## Jak To Zrobić

Powodem, dla którego porównywanie dat jest ważne w programowaniu, jest to, że często musimy wykonywać pewne operacje na danych, które są datami. Na przykład, możemy chcieć sprawdzić, który z dwóch wydarzeń nastąpił wcześniej, lub porównać daty w celu ustalenia, czy dana data jest w wyznaczonym przedziale czasowym. W języku Java istnieje wiele sposobów na porównywanie dat, ale skupimy się na dwóch najpopularniejszych metodach - za pomocą metod "compareTo" i "equals". 

```Java
import java.time.LocalDate; // importujemy klasę LocalDate z pakietu java.time

// tworzymy dwie zmienne typu LocalDate, reprezentujące dwie różne daty
LocalDate data1 = LocalDate.of(2021, 3, 26);
LocalDate data2 = LocalDate.of(2021, 3, 29);

// porównujemy daty za pomocą metody "compareTo"
int wynik = data1.compareTo(data2);
if(wynik < 0) {
  System.out.println(data1 + " jest wcześniej niż " + data2); // zostanie wyświetlony napis "2021-03-26 jest wcześniej niż 2021-03-29"
} else if(wynik == 0) {
  System.out.println(data1 + " jest równa " + data2); // zostanie wyświetlony napis "2021-03-26 jest równa 2021-03-29"
} else {
  System.out.println(data1 + " jest później niż " + data2); // zostanie wyświetlony napis "2021-03-26 jest później niż 2021-03-29"
}

// porównujemy daty za pomocą metody "equals"
if(data1.equals(data2)) {
  System.out.println(data1 + " jest równa " + data2); // nie zostanie wyświetlony, ponieważ daty są różne
} else {
  System.out.println(data1 + " jest różna od " + data2); // zostanie wyświetlony napis "2021-03-26 jest różna od 2021-03-29"
}
```

## Głębsza Stosowność

W języku Java daty są reprezentowane przez klasy LocalDate, LocalTime lub LocalDateTime, zależnie od tego, czy chcemy przechowywać tylko datę, czas lub oba. Te klasy mają bogatą funkcjonalność, która pozwala na wykonywanie różnych operacji na datach, w tym porównywanie ich. Warto przeczytać dokładną dokumentację tych klas, aby dowiedzieć się więcej o wszystkich dostępnych metodach.

## Zobacz również

- [Dokumentacja klasy LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [Dokumentacja klasy LocalTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalTime.html)
- [Dokumentacja klasy LocalDateTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDateTime.html)