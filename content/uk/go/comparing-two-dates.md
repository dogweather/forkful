---
title:    "Go: Порівняння двох дат"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Чому

Як дати є важливою частиною програмування та комп'ютерних наук, порівняння двох дат може бути корисним інструментом для багатьох різних задач. Наприклад, він може допомогти визначити різницю в часі між двома подіями або знайти найближчу майбутню дату. У цій статті ми розглянемо, як це зробити з допомогою мови програмування Go.

## Як це зробити

Найпростішим способом порівняти дві дати в Go є використання функції `Before()` або `After()`, яка повертає значення true або false в залежності від того, чи є перша дата раніше або пізніше за другу.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Порівняння дат
    date1 := time.Date(2020, time.January, 4, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, time.January, 8, 0, 0, 0, 0, time.UTC)

    if date1.Before(date2) {
        fmt.Println("Перша дата раніше за другу.")
    } else if date1.After(date2) {
        fmt.Println("Перша дата пізніше за другу.")
    } else {
        fmt.Println("Дати однакові.")
    }
    // Виведе: Перша дата раніше за другу.
}
```

За допомогою функції `Equal()` також можна знайти чи дати однакові.

```Go
if date1.Equal(date2) {
    fmt.Println("Дати однакові.")
}
```

Для більш точного порівняння, ви можете скористатися функцією `Before()` чи `After()` в поєднанні з `Sub()` для отримання різниці в часі між двома датами.

```Go
if date1.Before(date2) {
    difference := date2.Sub(date1)
    fmt.Printf("Перша дата раніше за другу на %d годин.", difference.Hours())
    // Виведе: Перша дата раніше за другу на 96 годин.
}
```

## Вдивіться глибше

У Go є також багато інших функцій для порівняння дат, таких як `EqualFold()` для порівняння двох дат з урахуванням часових зон та `Round()` для округлення дат до певних одиниць часу.

## Дивіться також

- [Офіційна документація Go з функціями для роботи з датами](https://golang.org/pkg/time/)
- [Стаття про порівняння дат у мові програмування Java](https://www.baeldung.com/java-compare-dates)
- [Перевірка суміщення періодів в Go](https://medium.com/@kumar303/go-how-to-check-if-two-time-periods-overlap-73c4c812b2ea)