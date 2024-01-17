---
title:                "Перетворення дати у рядок"
html_title:           "Kotlin: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Конвертація дати в рядок - це процес перетворення дати у формат, який ви можете використовувати для виведення на екран або збереження в базу даних. Програмісти часто це роблять, щоб полегшити спілкування з користувачами або роботою з даними у своїх програмах.

## Як це зробити:
Наступні приклади коду показують, як сконвертувати дату в рядок за допомогою мови Kotlin і вивести його у потрібному форматі:
```Kotlin
// Створення об'єкта дати
val date = Date()

// Форматування дати в рядок за допомогою простого шаблону
val simpleDateFormat = SimpleDateFormat("dd.MM.yyyy")
println(simpleDateFormat.format(date)) // вивід: 28.05.2020

// Форматування дати з додаванням назви дня тижня
val pattern = "EEEE, dd.MM.yyyy"
val dateFormat = SimpleDateFormat(pattern, Locale("uk", "UA"))
println(dateFormat.format(date)) // вивід: четвер, 28.05.2020

// Конвертація дати в рядок із заданою часовою зоной
val timeZone = TimeZone.getTimeZone("Europe/Kiev")
val timeZoneFormat = SimpleDateFormat("dd.MM.yyyy, HH:mm:ss", Locale("uk", "UA"))
timeZoneFormat.timeZone = timeZone
println(timeZoneFormat.format(date)) // вивід: 28.05.2020, 17:30:45
```

## Глибоке дослідження:
Дати використовуються у програмуванні вже протягом багатьох років і, звичайно, існує багато простіших способів робити це за допомогою стандартних функцій мови Kotlin, таких як ```toString()``` або ```LocalDate.parse()```. Однак, ручне форматування дозволяє більшу гнучкість при виведенні дат у потрібному форматі. Для конвертації дати в рядок також можна використовувати бібліотеки, наприклад, Joda Time або ThreeTen-Extra, які мають більше варіантів форматування та інші корисні функції.

## Додаткові посилання:
- Офіційна документація Kotlin: https://kotlinlang.org/
- Приклади конвертації дати в мові Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-date-format/
- Довідник по датам у Kotlin: https://www.baeldung.com/kotlin-date-time