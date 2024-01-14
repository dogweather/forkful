---
title:    "Java: Uzyskiwanie aktualnej daty"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Dlaczego pobrać bieżącą datę jest ważne?

Pobieranie bieżącej daty jest częstym zadaniem w wielu programach Java. Może być również przydatne w codziennej pracy programistów do wyświetlania daty wydarzenia lub okresu ważności dokumentów. W tym artykule dowiesz się, jak można łatwo pobrać bieżącą datę w swoim kodzie Java.

## Jak to zrobić?

Najprostszym sposobem na uzyskanie bieżącej daty w Javie jest użycie klasy `LocalDate` z pakietu `java.time`. Wystarczy utworzyć obiekt tej klasy i wywołać jego metodę `now()`, a następnie wyświetlić ją przy użyciu metody `toString()`. Poniżej znajduje się przykładowy kod:

```Java
import java.time.LocalDate;

public class CurrentDateExample{
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("Bieżąca data: " + currentDate.toString());
    }
}
```

Po uruchomieniu tego kodu, powinieneś otrzymać bieżącą datę w formacie RRRR-MM-DD, na przykład `2021-03-23`.

Jeśli chcesz wyświetlić bieżącą datę w innym formacie, możesz skorzystać z metody `format()` z klasy `DateTimeFormatter`. Przykładowy kod może wyglądać następująco:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CurrentDateExample{
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/RRRR");
        System.out.println("Bieżąca data w formacie dd/MM/RRRR: " + formatter.format(currentDate));
    }
}
```

## Głębsze zanurzenie

Klasa `LocalDate` oferuje również wiele innych metod, które umożliwiają manipulowanie datami. Na przykład, możesz wyświetlić tylko rok, miesiąc lub dzień, wykonać operacje matematyczne na dacie, lub nawet sprawdzić, czy dana data jest przed lub po innej dacie. Aby dowiedzieć się więcej, zapoznaj się z dokumentacją klasy `LocalDate`.

## Zobacz także

- Dokumentacja klasy `LocalDate`: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Poradnik o pakiecie `java.time`: https://www.baeldung.com/java-8-date-time-intro
- Przykładowe zadania związane z datami w Javie: https://www.hackerrank.com/domains/java/java-date-and-time