---
title:    "Java: Konwertowanie daty na ciąg znaków"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest niezwykle przydatnym zadaniem w programowaniu, ponieważ pozwala na wyświetlanie dat w czytelny sposób dla użytkowników lub zapisywanie ich w bazie danych. Jest to także integralna część wielu złożonych aplikacji, ponieważ pozwala na przetwarzanie i manipulację danymi o czasie.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w języku Java, należy użyć klasy SimpleDateFormat. Najpierw należy utworzyć obiekt tej klasy, podając jako argument wybrany format, w którym chcemy wyświetlić datę. Następnie używając metody format, przekazujemy jako argument obiekt klasy Date, który chcemy przekonwertować. Oto prosty przykład:

```Java
// Importowanie potrzebnej klasy
import java.text.SimpleDateFormat;

// Utworzenie obiektu klasy SimpleDateFormat z wybranym formatem
SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");

// Utworzenie obiektu klasy Date
Date today = new Date();

// Przekonwertowanie daty na ciąg znaków
String dateString = dateFormat.format(today);

// Wyświetlenie rezultatu
System.out.println(dateString); // Wynik: 15-04-2021
```

W powyższym przykładzie użyliśmy formatu "dd-MM-yyyy", ale istnieje wiele innych dostępnych formatów, takich jak "yyyy-MM-dd", "E, dd.MM.yyyy" czy "MMM dd, yyyy". Szczegółowe informacje na temat różnych formatów można znaleźć w dokumentacji klasy SimpleDateFormat.

## Głębsze zagadnienia

Warto mieć na uwadze, że konwersja daty na ciąg znaków jest dwukierunkowym procesem. Oznacza to, że nie tylko można przekonwertować obiekt klasy Date na ciąg znaków, ale również można odwrócić ten proces, przekonwertowując ciąg znaków z powrotem na obiekt Date. W tym celu można użyć metody parse klasy SimpleDateFormat, podając jako argument ciąg znaków i format, który został użyty do konwersji.

## Zobacz także

- Dokumentacja klasy SimpleDateFormat: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
- Przykładowe formaty daty: https://www.tutorialspoint.com/java/java_date_time.htm
- Tutorial o konwersji daty na ciąg znaków i vice versa: https://www.baeldung.com/java-date-conversion