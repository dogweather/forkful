---
title:                "Porównywanie dwóch dat"
html_title:           "Java: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest częstym zadaniem, które pojawia się podczas programowania w Javie. Jest to szczególnie przydatne, kiedy chcemy sprawdzić, która z dwóch dat jest późniejsza lub wcześniejsza, lub porównać je z bieżącym czasem. W tym artykule dowiesz się, jak porównywać daty w języku Java i jak to może ułatwić Ci prace z tymi wieloma liczbami.

## Jak porównywać daty w Javie

Porównywanie dwóch dat w Javie jest proste i można to zrobić na kilka sposobów. Oto kilka przykładów kodu, które pokażą Ci, jak to zrobić.

```Java
// importujemy klasę Date z pakietu java.util
import java.util.Date;

// tworzymy dwa obiekty typu Date, każdy z inną datą
Date data1 = new Date(2021, 1, 1); // data: 1 stycznia 2021
Date data2 = new Date(2022, 1, 1); // data: 1 stycznia 2022

// używamy metody compareTo(), która zwraca wartość ujemną,
// jeśli data1 jest wcześniejsza od data2,
// wartość dodatnią, jeśli data1 jest późniejsza od data2,
// lub 0, jeśli daty są sobie równe
int result = data1.compareTo(data2); // result = -1
```

W tym przykładzie wykorzystaliśmy metodę `compareTo()` klasy `Date`, która jest przeciążona i przyjmuje jako argument inny obiekt typu `Date`. Metoda ta zwraca liczbę całkowitą, która jest wartością porównania. W tym przypadku -1 oznacza, że data1 jest wcześniejsza od data2.

Możemy również skorzystać z metody `before()` lub `after()`, aby sprawdzić, czy jedna data jest wcześniejsza lub późniejsza od drugiej.

```Java
// importujemy klasę Calendar z pakietu java.util
import java.util.Calendar;

// tworzymy dwa obiekty typu Calendar, każdy z inną datą
Calendar kalendarz1 = Calendar.getInstance();
kalendarz1.set(2021, 1, 1); // ustawiamy datę: 1 stycznia 2021
Calendar kalendarz2 = Calendar.getInstance();
kalendarz2.set(2022, 1, 1); // ustawiamy datę: 1 stycznia 2022

// używamy metody before(), która zwraca true,
// jeśli data z pierwszego kalendarza jest wcześniejsza
// od daty z drugiego kalendarza
boolean przed = kalendarz1.before(kalendarz2); // przed = true
```

W powyższym przykładzie wykorzystujemy obiekty typu `Calendar`, które pozwalają nam łatwo manipulować datami i czasem. Metoda `before()` zwraca wartość logiczną (true lub false), co pozwala nam sprawdzić, czy data z pierwszego kalendarza jest wcześniejsza od daty z drugiego kalendarza.

Możemy również użyć metody `compareTo()` klasy `LocalDate`, która jest dostępna od wersji Java 8.

```Java
// importujemy klasę LocalDate z pakietu java.time
import java.time.LocalDate;

// tworzymy dwa obiekty typu LocalDate, każdy z inną datą
LocalDate localDate1 = LocalDate.of(2021, 1, 1); // data: 1 stycznia 2021
LocalDate localDate2 = LocalDate.of(2022, 1, 1); // data: 1 stycznia 2022

// używamy metody compareTo(), która zwraca wartość ujemną,
// jeśli localDate1 jest wcześniejsze od localDate2,
// wartość dodatnią, jeśli localDate1 jest późniejsze od