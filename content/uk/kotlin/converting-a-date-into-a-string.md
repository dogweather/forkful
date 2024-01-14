---
title:    "Kotlin: Перетворення дати у рядок"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Чому
Конвертація дати в рядок є важливою технікою, оскільки часто необхідно вивести дату у зрозумілому для користувача форматі, наприклад, для відображення на інтерфейсі або для збереження у базі даних.

## Як
Для перетворення дати в рядок у Kotlin існує декілька способів. Нижче наведені приклади коду та вихідний результат для кожного з них.

```Kotlin
// Створення об'єкту з поточною датою
val currentDate = LocalDate.now()

// Виведення дати у форматі "день-місяць-рік"
val formattedDate = currentDate.format(DateTimeFormatter.ofPattern("dd-MM-yyyy"))
println(formattedDate) // Виведе: 23-09-2021

// Виведення дати у форматі "рік-місяць-день"
val formattedDate2 = currentDate.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
println(formattedDate2) // Виведе: 2021-09-23

// Виведення дати у форматі "місяць-рік-день"
val formattedDate3 = currentDate.format(DateTimeFormatter.ofPattern("MM-yyyy-dd"))
println(formattedDate3) // Виведе: 09-2021-23
```

## Deep Dive
Для конвертації дати в рядок у Kotlin використовується об'єкт DateTimeFormatter. Він дозволяє задати необхідний формат виведення дати за допомогою шаблонів. Наприклад "dd" - день, "MM" - місяць, "yyyy" - рік. Також можна задати розширені налаштування, наприклад, використовувати 12 чи 24-годинний формат часу.

## See Also
- [Kotlin Date and Time](https://kotlinlang.org/docs/datetime.html)
- [DateTimeFormatter Class](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)