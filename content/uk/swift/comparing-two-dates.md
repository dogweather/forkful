---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат - це процес визначення, яка дата є ранішою, пізнішою або чи дати є рівними. Програмісти роблять це, щоби їм стежити за порядком подій, контролювати тайминг або встановити таймери. 

## Як це зробити:

Ми використовуємо вбудовані методи `compare(:)` або `isEarlier(than:)`/`isLater(than:)`.

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd HH:mm"
let date1 = formatter.date(from: "2022/02/21 10:00")!
let date2 = formatter.date(from: "2022/02/21 12:00")!

if(date1.compare(date2) == .orderedAscending) {
    print("Дата1 раніше Дати2")
} else if(date1.compare(date2) == .orderedSame) {
    print("Дата1 і Дата2 рівні")
} else {
    print("Дата1 пізніше Дати2")
}

if(date1.isEarlier(than: date2)) {
    print("Дата1 раніше Дати2")
}

if(date1.isLater(than: date2)) {
    print("Дата1 пізніше Дати2")
}
```
Цей код виведе:
```
Дата1 раніше Дати2
Дата1 раніше Дати2
```
## Поглиблений огляд

В історичному контексті, при порівнянні дат раніше використовувалися довгі, складні підходи. Але з введенням вбудованих функцій в Swift стало значно простіше і ефективніше.

Одна з альтернатив - використання вбудованої функції timeIntervalSince(), яка повертає різницю між двома датами в секундах. Якщо різниця від'ємна, перша дата раніша.

Перелічення `.orderedAscending`, `.orderedSame` та `.orderedDescending` - це частка спільного интерфейсу, виработаного Apple для порівняння.

## Дивитись також

Для детальнішої інформації дивіться офіційну документацію Swift про працю з `Date` та `DateFormatter`:
1. [Дата та Час в Swift](https://developer.apple.com/documentation/foundation/date)
2. [Форматтер Дати](https://developer.apple.com/documentation/foundation/dateformatter)
3. [Порівняння Об'єктів в Swift](https://developer.apple.com/documentation/swift/comparing_data)