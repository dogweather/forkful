---
title:                "Порівняння двох дат"
html_title:           "Java: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Зчому

Компарування двох дат - важливий аспект програмування в Java, оскільки це дозволяє визначити, яка з дат є більшою або меншою. Це корисно для сортування даних, роботи з різними форматами дат і багатьох інших завдань.

## Як це зробити

Для порівняння двох дат використовується метод "compareTo()", який повертає ціле число. Додатнє число означає, що перша дата більша за другу, від'ємне - що друга дата більша за першу і 0 - що дати рівні.

```Java
import java.time.LocalDate;

public class CompareDates {

    public static void main(String[] args) {

        // створення першої дати
        LocalDate date1 = LocalDate.of(2020, 1, 1);

        // створення другої дати
        LocalDate date2 = LocalDate.of(2021, 1, 1);

        // порівняння дат
        int result = date1.compareTo(date2);

        // відображення результатів
        System.out.println("Дати з базового року: " + result);
    }
}
```

Результат виконання програми:

```
Дати з базового року: -365
```

## Profundum

Окрім "compareTo()", для порівняння дат також можна використовувати методи "isBefore()" і "isAfter()", які повертають логічну значення (true або false). Також, для зручного порівняння дат, можна перетворити їх в числові значення за допомогою методу "toEpochDay()", а потім працювати з отриманими числами.

## Дивись також

- [Офіційна документація Java](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Date.html)
- [Стаття на Medium про порівняння дат в Java](https://medium.com/@erikdkennedy/comparing-dates-in-java-88068db433b1)