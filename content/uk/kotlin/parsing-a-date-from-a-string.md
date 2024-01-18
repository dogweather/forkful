---
title:                "Розбір дати з рядка"
html_title:           "Kotlin: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Що і чому?
Значення розбору дати з рядка і що на це спонукає програмістів. Розбір дат з рядка - це процес перетворення дати з формату рядка в об'єкт дати, який можна рухати, виправляти та форматувати зручним для програміста способом. Програмісти використовують цей процес для обробки та збереження дат у своїх програмах.

Як це зробити:
```Kotlin
val dateString = "24/08/2021"
val format = SimpleDateFormat("dd/MM/yyyy", Locale.ENGLISH)
val date = format.parse(dateString)
println("Parsed date: $date")
```

Результат виконання:
```
Parsed date: Tue Aug 24 00:00:00 EEST 2021
```

Глибокий погляд:
Розбір дати з рядка є важливим процесом у програмуванні та має багато застосувань. Історично, розбір дат з рядка був складною задачею, але з появою сучасних бібліотек та мов програмування, цей процес став значно простішим. Існує також кілька альтернативних підходів до розбору дат з рядка, таких як використання регулярних виразів або спеціальних бібліотек.

Пов'язані посилання:
- [Документація з Kotlin про розбір дат з рядка](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.text.-simple-date-format/)
- [Порівняння різних підходів до розбору дат з рядка](https://www.baeldung.com/java-string-to-date)
- [Приклади використання бібліотеки Joda-Time для розбору дат з рядка](https://www.baeldung.com/joda-time)