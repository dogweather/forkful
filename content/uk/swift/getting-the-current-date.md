---
title:                "Swift: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Будь-який програміст, безумовно, знає, що отримання поточної дати є важливою задачею при розробці додатків. Для чого це потрібно? Тут ми розглянемо це питання і дамо деякі приклади використання.

## Як виконати це завдання

```Swift
// Отримуємо поточну дату в форматі року, місяця та дня
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let dateString = dateFormatter.string(from: currentDate)
print(dateString)
```

```Swift
// Отримуємо поточний час в форматі годин, хвилин та секунд
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "HH:mm:ss"
let timeString = dateFormatter.string(from: currentDate)
print(timeString)

```

```Swift
// Отримуємо повний формат дати та часу
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .full
dateFormatter.timeStyle = .full
let dateTimeString = dateFormatter.string(from: currentDate)
print(dateTimeString)
```

Всі ці кодові приклади показують різні формати отримання поточної дати і часу. Вони дають можливість використовувати їх для власних потреб. Також існує багато інших форматів, які можна використовувати за допомогою класу DateFormatter.

## Глибокий аналіз

В цьому розділі ми розглянемо детальніше процес отримання поточної дати в Swift. У документації Apple застосовується термін "часова точка" для позначення визначення дати і часу. Це може здатися складною концепцією, але насправді це просто стан календаря в певний момент часу.

У Swift існує клас Date, який забезпечує можливість працювати з часовими точками. Він використовує тип даних TimeInterval для представлення часу в секундах, що пройшли з 1 січня 2001 року. Також існує багато допоміжних методів для роботи з датами, таких як додавання або віднімання певної кількості секунд.

## Дивись також

- [Робота з датами і часом в Swift](https://www.raywenderlich.com/3932-dates-and-times-in-swift-getting-started-with-foundation)
- [Офіційна документація Apple](https://developer.apple.com/documentation/foundation/date)