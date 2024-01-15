---
title:                "Порівняння двох дат"
html_title:           "Go: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні часто доводиться порівнювати дати. Наприклад, перевіряти, чи одна дата наступає після іншої, визначати різницю між датами або порівнювати часові проміжки. Використовуючи мову програмування Go, це можна зробити дуже ефективно і просто. Ця стаття покаже вам, як саме це зробити.

## Як це зробити

```Go
var date1 = time.Date(2021, time.March, 20, 0, 0, 0, 0, time.UTC)
var date2 = time.Date(2021, time.April, 15, 0, 0, 0, 0, time.UTC)

//Перевірка, чи дата date1 наступає після дати date2
if date1.After(date2) {
	fmt.Println("Дата 1 наступає після дати 2")
}

// Визначення різниці між датами
difference := date2.Sub(date1)
fmt.Println("Різниця між датами:", difference)

// Порівняння часових проміжків
var duration1 = time.Duration(72) * time.Hour
var duration2 = time.Duration(3) * time.Hour

if duration1 > duration2 {
	fmt.Println("Тривалість 1 більша за тривалість 2")
}
```

Виходи програми:

```
Дата 1 наступає після дати 2
Різниця між датами: 720h0m0s
Тривалість 1 більша за тривалість 2
```

## Глибші відомості

У Go зручно порівнювати дати за допомогою методів `time.Before()`, `time.After()` та `time.Equal()`. Крім цього, можна використовувати метод `Sub()` для визначення різниці між двома датами. Якщо потрібно порівняти часові проміжки, то слід використовувати тип `time.Duration` та методи `>`, `<` або `=`.

## Дивіться також

- [Офіційна документація з порівняння дат](https://golang.org/pkg/time/)
- [Уроки з мови Go на сайті Codeacademy](https://www.codecademy.com/learn/learn-go)