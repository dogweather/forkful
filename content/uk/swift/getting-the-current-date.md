---
title:                "Swift: Отримання поточної дати."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому 

Отримання поточної дати є важливою функцією у програмуванні для великої кількості сценаріїв. Наприклад, ви можете використовувати поточну дату для показу часу створення, розміщення або сповіщення про подію. 

## Як

Для отримання поточної дати використовуйте клас `Date`. Перш за все, необхідно імпортувати фреймворк `Foundation` у свою програму. Потім можна використовувати метод `Date()` для створення об'єкта `Date`, який буде містити поточну дату. Нижче наведено приклад коду та його вихідного результату: 

```Swift 
import Foundation 

let currentDate = Date() 
print(currentDate) 
``` 

Виведе: `2021-02-08 19:12:56 +0000` 

Також, можна використовувати клас `DateFormatter`, щоб отримати поточну дату у більш зручному для користувача форматі. Наприклад, можна вивести дату у зрозумілому форматі користувачу своєї країни. Нижче наведено приклад коду та його вихідного результату: 

```Swift 
import Foundation 

let dateFormatter = DateFormatter() 
dateFormatter.dateFormat = "dd/MM/yyyy" 

let currentDate = Date() 
print(dateFormatter.string(from: currentDate)) 
``` 

Виведе: `08/02/2021` 

## Глибока занурення 

В класі `Date` також доступні методи для отримання специфічних значень, таких як день, місяць, рік та час. Наприклад, `dateComponents()` дозволяє отримати компоненти дати, такі як день тижня, місяць та рік. Для отримання більш детальної інформації про ці методи, можна переглянути [документацію](https://developer.apple.com/documentation/foundation/date) по класу `Date`.

## Дивись також 

- [Стаття про клас Date у мові Swift](https://www.ioscreator.com/tutorials/get-current-date)
- [Документація Apple Developer про клас Date](https://developer.apple.com/documentation/foundation/date)