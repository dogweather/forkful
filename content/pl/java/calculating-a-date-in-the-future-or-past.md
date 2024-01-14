---
title:    "Java: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Wyobraź sobie, że musisz zaplanować wakacje w przyszłym roku lub stworzyć harmonogram spotkań biznesowych na kolejne miesiące. W takich sytuacjach często musimy wyznaczyć daty w przyszłości lub w przeszłości. W tym przypadku warto poznać sposoby obliczania daty w przód lub w tył za pomocą Javy.

## Jak to zrobić

Poniżej przedstawiamy przykładowy kod w Javie, który pozwoli na obliczenie daty w przód lub w tył.

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculator {

    public static void main(String[] args) {
        
        // Wyznaczenie daty za n lat
        int years = 2;
        LocalDate futureDate = LocalDate.now().plusYears(years);
        
        // Formatowanie daty do odpowiedniego formatu
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String formattedDate = futureDate.format(formatter);
        
        System.out.println("Za " + years + " lata będzie " + formattedDate);
        
        // Wyznaczenie daty sprzed n dni
        int days = 10;
        LocalDate pastDate = LocalDate.now().minusDays(days);
        
        // Formatowanie daty do odpowiedniego formatu
        String formattedPastDate = pastDate.format(formatter);
        
        System.out.println("10 dni temu było " + formattedPastDate);
        
        // Wyznaczenie daty za n miesięcy i n dni
        int months = 3;
        int additionalDays = 20;
        LocalDate futureDateWithDays = LocalDate.now().plusMonths(months).plusDays(additionalDays);
        
        // Formatowanie daty do odpowiedniego formatu
        String formattedFutureDateWithDays = futureDateWithDays.format(formatter);
        
        System.out.println("Za " + months + " miesięcy i " + additionalDays + " dni będzie " + formattedFutureDateWithDays);
    }
}
```

**Output:**
Za 2 lata będzie 29/07/2023
10 dni temu było 19/07/2021
Za 3 miesięcy i 20 dni będzie 18/11/2021

## Głębsza analiza

W Javie istnieje wiele metod i klas, które umożliwiają obliczanie daty w przyszłości lub w przeszłości. W powyższym przykładzie wykorzystaliśmy metodę `plusYears()` do obliczenia daty za n lat, `minusDays()` do wyznaczenia daty sprzed n dni oraz `plusMonths()` i `plusDays()` do wyznaczenia daty za n miesięcy i n dni. Warto dokładniej przyjrzeć się tym metodom i innym sposobom obliczania daty w Javie, aby jeszcze bardziej usprawnić nasze działania.

## Zobacz też

- [Java LocalDate Dokumentacja](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java DateTimeFormatter Dokumentacja](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Tutorial: Obliczanie daty za pomocą klasy Calendar w Javie](https://www.baeldung.com/java-date-calendar)