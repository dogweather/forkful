---
title:                "Java: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому
Дати є важливою концепцією в більшості програмування, і іноді ми можемо зіткнутися з потребою порівнювати дві дати. Наприклад, ми можемо потребувати перевірити, чи є одна дата після іншої або чи вони однакові. У цьому блозі ми поговоримо про те, як порівнювати дві дати в Java та чому це може бути корисно для вашого програмування.

## Як це робити
Розглянемо найпоширеніші методи порівняння дат в Java.

### Порівняння за допомогою методу compare()
Метод compare() в класі LocalDate дозволяє порівняти дві дати. Він повертає значення 0, якщо дати однакові, значення більше 0, якщо перша дата пізніша за другу, і значення менше 0, якщо перша дата раніше за другу. Наприклад:

```Java
LocalDate date1 = LocalDate.of(2020, 5, 15);
LocalDate date2 = LocalDate.of(2020, 5, 20);

int result = date1.compare(date2);

if (result == 0) {
    System.out.println("Both dates are equal");
} else if (result < 0) {
    System.out.println("Date 1 is before date 2");
} else {
    System.out.println("Date 1 is after date 2");
}

// Output: Date 1 is before date 2
```

### Порівняння за допомогою методів isAfter() та isBefore()
Крім методу compare(), клас LocalDate також має методи isAfter() та isBefore(), які дозволяють перевірити, чи є одна дата пізнішою або ранішою за іншу. Наприклад:

```Java
LocalDate date1 = LocalDate.of(2020, 5, 15);
LocalDate date2 = LocalDate.of(2020, 5, 20);

if (date1.isBefore(date2)) {
    System.out.println("Date 1 is before date 2");
}

if (date2.isAfter(date1)) {
    System.out.println("Date 2 is after date 1");
}

// Output: Date 1 is before date 2
// Date 2 is after date 1
```

## Занурення в тему
Існує багато методів порівняння дат в Java, і ви можете вибрати той, який найкраще підходить для вашої програми. Наприклад, клас LocalDateTime має методи isAfter(), isBefore() та isEqual(), які дозволяють порівняти не тільки дати, але і час. Також можна використовувати клас Duration для порівняння часових проміжків між датами.

## Дивись також
- [Документація Java для порівняння дат](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#method.summary)
- [Відео про порівняння дат в Java](https://www.youtube.com/watch?v=Puz4Nx0zyAs)
- [Стаття про роботу з датами та часом в Java](https://www.baeldung.com/java-dates)