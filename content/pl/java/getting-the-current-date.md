---
title:                "Java: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto pobrać aktualną datę w programowaniu? Wiele aplikacji wymaga określenia daty i czasu, na przykład przy tworzeniu logów lub generowaniu raportów. W tym artykule pokażę Wam jak w prosty sposób pobrać aktualną datę w języku Java.

## Jak to zrobić

Aby pobrać aktualną datę w Javie, należy skorzystać z klasy "java.util.Date". Najprostszym sposobem jest użycie metody "new Date()", która zwraca obiekt klasy Date z aktualną datą i czasem. Oto przykładowy kod:

```Java
import java.util.Date;

public class CurrentDate {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println("Aktualna data i czas: " + currentDate);
    }
}
```
Wynik działania programu będzie wyglądał następująco:
```
Aktualna data i czas: Sat Nov 07 19:23:45 CET 2020
```

Jeśli chcemy uzyskać tylko datę lub czas, możemy skorzystać z metod "getDate()" i "getTime()" klasy Date. Oto przykład kodu:

```Java
import java.util.Date;

public class CurrentDateTime {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println("Aktualna data: " + (currentDate.getDate() + 1) + "/" + (currentDate.getMonth() + 1) + "/" + (currentDate.getYear() + 1900));
        System.out.println("Aktualny czas: " + currentDate.getHours() + ":" + currentDate.getMinutes() + ":" + currentDate.getSeconds());
    }
}
```
Wynik działania programu będzie wyglądał następująco:
```
Aktualna data: 8/11/2020
Aktualny czas: 19:23:45
```

## Głębsze zanurzenie

W poprzednich przykładach skorzystaliśmy z klasy "java.util.Date" do pobrania aktualnej daty. Jednak warto zauważyć, że klasa ta została oznaczona jako przestarzała, a zaleca się korzystanie z klasy "java.time.LocalDate" lub "java.time.LocalDateTime" (dostępne od JDK 8). Korzystając z tych klas, możemy wybrać konkretną strefę czasową oraz format daty i czasu.

Przykładowy kod wykorzystujący klasę "java.time.LocalDate":

```Java
import java.time.LocalDate;

public class CurrentDate {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("Aktualna data: " + currentDate);
    }
}
```
Wynik działania programu będzie wyglądał następująco:
```
Aktualna data: 2020-11-08
```

Aby skorzystać z klasy "java.time.LocalDateTime" i wybrać strefę czasową oraz format daty i czasu, możemy użyć klasy "java.time.format.DateTimeFormatter". Oto przykładowy kod:

```Java
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class CurrentDateTime {
    public static void main(String[] args) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneId.of("Europe/Warsaw"));
        DateTimeFormatter format = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");
        String formattedDate = currentDate.format(format);
        System.out.println("Aktualna data i czas: " + formattedDate);
    }
}
```
Wynik działania programu będzie wyglądał następująco:
```
Aktualna data i czas: 08-11-2020 19:23:45
```

## Zobacz także

- [Oracle: Class Date] (https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [Oracle: Package java.time] (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java - Materiały online] (https://www.java.com/pl/download/help/resources.xml)