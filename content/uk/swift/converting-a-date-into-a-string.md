---
title:                "Перетворення дати в рядок"
html_title:           "Swift: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Для чого: Якщо ви хочете показати дату в зручному форматі для користувача, необхідно конвертувати її в рядок.

Як: У цій статті ми покажемо вам, як конвертувати дату в рядок за допомогою мови програмування Swift.

``` Swift
let date = Date() //створюємо змінну для дати
let formatter = DateFormatter() //створюємо форматтер для дати
formatter.dateFormat = "dd/MM/yyyy" //задаємо потрібний формат дати
let stringDate = formatter.string(from: date) //конвертуємо дату в рядок
print(stringDate) //виводимо результат в консоль
```

Ви введете цей код в Playgrounds і побачите, що поточна дата буде виведена у форматі "день/місяць/рік".

Ось ще кілька корисних прикладів форматів для дати:

- "dd-MM-yyyy" - 31-12-2021
- "MMMM dd, yyyy" - December 31, 2021
- "EEEE, MMM d, yyyy" - Saturday, Dec 31, 2021

Глибоке занурення: Для тих, хто хоче дізнатися більше, ми поговоримо про методи та опції, які можна використовувати при конвертації дати в рядок.

``` Swift
//створюємо змінну з поточною датою
let date = Date()

//створюємо форматтер для дати з шаблоном "dd/MM/yyyy"
let formatter = DateFormatter()

//задаємо налаштування для форматтера
formatter.locale = Locale(identifier: "uk_UA") //використовуємо локаль "uk_UA" для української мови
formatter.dateFormat = "dd/MM/yyyy"

//конвертуємо дату в рядок за допомогою методу string(from:)
let stringDate = formatter.string(from: date)

//виводимо результат
print(stringDate) //31/12/2021

//Також, ви можете вказати додаткові деталі для форматування дати, наприклад:
formatter.timeZone = TimeZone(secondsFromGMT: 0) //встановлюємо часовий пояс
formatter.amSymbol = "AM" //замінюємо значення для першої половини дня
formatter.pmSymbol = "PM" //замінюємо значення для другої половини дня
```

Також, є можливість використовувати опції для конвертації дати в рядок з допомогою методу string(from:):

- ".short" - короткий формат дати (13/12/21)
- ".medium" - середній формат дати (13 груд. 2021 р.)
- ".long" - довгий формат дати (13 грудня 2021 р.)
- ".full" - повний формат дати (субота, 13 грудня 2021 р.)

Тепер ви знаєте, як конвертувати дату в рядок в мові Swift! Сподіваємося, ця стаття стала корисною для вас.

Дивіться також: 

- [Прост