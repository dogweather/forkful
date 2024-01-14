---
title:                "Swift: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## З чого?

Змінювати дату у рядок може бути корисно, коли ви хочете відображати дату у вигляді тексту, наприклад, для створення унікальних ідентифікаторів або для виведення дати у зручному форматі для користувача.

## Як

``` Swift
let currentDate = Date() //отримуємо поточну дату
let dateFormatter = DateFormatter() //створюємо об'єкт DateFormatter
dateFormatter.dateFormat = "dd-MM-yyyy" //встановлюємо формат дати
let dateString = dateFormatter.string(from: currentDate) //конвертуємо дату у рядок
print(dateString) //виводимо результат: 25-09-2021
```

``` Swift
let sampleDate = "2021-10-31" //задаємо приклад дати у форматі "рік-місяць-день"
let dateFormatter = DateFormatter() //створюємо об'єкт DateFormatter
dateFormatter.dateFormat = "yyyy-MM-dd" //встановлюємо формат дати для розбору
if let date = dateFormatter.date(from: sampleDate) { //перевіряємо чи встановлений формат співпадає зі змінною
  dateFormatter.dateStyle = .medium //встановлюємо стиль для виводу дати
  let dateString = dateFormatter.string(from: date) //конвертуємо дату у рядок
  print(dateString) //виводимо результат: Oct 31, 2021
} else {
  print("Invalid date format") //виводимо повідомлення про недійсний формат дати
}
```

## Глибинний аналіз

Для конвертування дати в рядок використовуються класи `Date` і `DateFormatter`. Для встановлення формату дати використовуються вирази у форматі `yyyy` для року, `MM` для місяця, `dd` для дня тощо. `DateFormatter` також має різні стилі для виводу дати у зручному для користувача форматі.

## Дивіться також

- [NSDateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [Working with Dates in Swift - Ray Wenderlich](https://www.raywenderlich.com/760-a-quick-look-at-dates-in-swift)
- [Date and Time Programming Guide - Apple Developer Documentation](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)