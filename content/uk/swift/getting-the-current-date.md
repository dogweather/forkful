---
title:    "Swift: Отримання поточної дати"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Чому

Отримання поточної дати є важливим аспектом програмування для багатьох розробників. Це дозволяє зберігати та відстежувати дані із датами, що лежать у основі багатьох архітектурних рішень.  

# Як це зробити

Найпростіший спосіб отримати поточну дату в Swift - використання класу `Date`. Ось приклад коду та виведення: 

```Swift
let now = Date()
print(now)
```

Результат виведення:

`2021-05-12 18:30:00 +0000`

Крім того, можна отримати дату у потрібному форматі за допомогою `DateFormatter`. Ось приклад коду та виведення у форматі дня, місяця, року:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
print(dateFormatter.string(from: now))
```

Результат виведення:

`12.05.2021`

# Глибше дослідження

Хоча отримання поточної дати може здатися простою задачею, воно дещо складніше виконати. Наприклад, враховуючи часові зони та локалізацію. У Swift є гарні засоби для роботи з цими аспектами, такі як `TimeZone` та `Locale`. При необхідності, можна відобразити дату у потрібній часовій зоні та мові.

# Дивись також

- [Офіційна документація Swift](https://swift.org/documentation/)
- [Відеоуроки з основ Swift](https://www.youtube.com/playlist?list=PL2NJyRY6DNuWGYe5SqTfCx168gkZP7aWl)
- [Стаття про роботу з датами в Swift](https://www.raywenderlich.com/718101-swift-date-cheat-sheet-getting-started-with-dates-in-swift)