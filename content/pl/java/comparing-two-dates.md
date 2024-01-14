---
title:    "Java: Porównywanie dwóch dat"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego?

Porównywanie dat może być przydatne w wielu sytuacjach w programowaniu. Na przykład, można użyć tej funkcji do sprawdzenia, która data jest wcześniejsza lub późniejsza, co może pomóc w ustaleniu kolejności zdarzeń lub wykryciu błędów w danych. Jest to także często używane do obsługi harmonogramów lub wyświetlania dat w odpowiedni sposób dla użytkowników.

## Jak to zrobić?

Aby porównać dwie daty w języku Java, musisz użyć metody "compareTo" z klasy "Date". Przykładowy kod poniżej pokazuje, jak to zrobić oraz jakie wartości wyjściowe można otrzymać:

```Java
import java.util.Date;

public class ComparingDates {
    public static void main(String[] args) {
        // Tworzenie dwóch dat
        Date firstDate = new Date(2021, 0, 1); // 1 stycznia 2021
        Date secondDate = new Date(); // bieżąca data i czas

        // Porównywanie dat przy użyciu metody compareTo
        int comparison = firstDate.compareTo(secondDate);

        // Wyświetlanie wyniku
        if (comparison > 0) {
            System.out.println(firstDate + " jest późniejsza niż " + secondDate);
        } else if (comparison < 0) {
            System.out.println(firstDate + " jest wcześniejsza niż " + secondDate);
        } else {
            System.out.println("Obie daty są takie same.");
        }
    }
}
```

Przykładowy wynik:

```
1 stycznia 2021 jest wcześniejsza niż Thu Aug 19 21:30:48 CEST 2021
```

Mając na uwadze powyższy kod, możesz zacząć eksperymentować z różnymi datami i sprawdzać, jakie wyniki otrzymujesz.

## Głębsze wgląd

Funkcja "compareTo" porównuje daty na podstawie ich wartości liczbowych, co oznacza, że pierwsza data jest wcześniejsza, niż druga, jeśli ma mniejszą wartość liczbową. Można także użyć metody "before" i "after" do porównywania dat, a wynikiem będzie wartość logiczna true lub false. Istnieje także możliwość wykorzystania klasy "LocalDate" z pakietu "java.time" do porównywania dat na podstawie konkretnych kryteriów, takich jak rok, miesiąc czy dzień.

## Zobacz także

- [Dokumentacja Java - klasa Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Porównywanie dat w języku Java - tutorial](https://www.baeldung.com/java-compare-dates)
- [Klasa LocalDate - dokumentacja](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)