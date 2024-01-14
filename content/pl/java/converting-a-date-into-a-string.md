---
title:                "Java: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest częstym zadaniem w programowaniu, szczególnie w aplikacjach związanych z zarządzaniem czasem i wymianą danych. Jest to przydatna umiejętność, która pozwala na wyświetlanie dat w różnych formatach lub przechowywanie ich w bazach danych.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w języku Java, możemy skorzystać z metody `format()` klasy `SimpleDateFormat`. Poniżej przedstawiamy przykład kodu oraz wynik, który otrzymamy:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

Date date = new Date();
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
String dateString = dateFormat.format(date);
System.out.println(dateString);
```

Output: 28/08/2021

W powyższym przykładzie użyliśmy specyficznego formatu "dd/MM/yyyy", ale istnieje wiele innych opcji, takich jak "MM/dd/yyyy" czy "dd-MMM-yyyy", które pozwalają na wyświetlanie daty w różnych stylach.

## Deep Dive

Warto zauważyć, że metoda `format()` również może przyjmować obiekt `Date` jako argument, co umożliwia konwersję daty zapisanej w innym formacie. Ponadto, istnieje możliwość sformatowania daty z uwzględnieniem strefy czasowej czy ustawienia odpowiedniego języka dla miesięcy.

W celu uzyskania głębszego zrozumienia konwertowania daty na ciąg znaków, warto zapoznać się z dokumentacją Java oraz przećwiczyć różne przypadki w praktyce.

## Zobacz też

- [Java Documentation](https://docs.oracle.com/javase/10/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial: Working with Dates in Java](https://www.baeldung.com/java-date-to-string-conversion)
- [How to Convert Date to String in Java](https://www.javacodeexamples.com/convert-date-to-string-in-java/1364)