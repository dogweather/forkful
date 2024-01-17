---
title:                "Використання регулярних виразів"
html_title:           "Kotlin: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Що і для чого?
Регулярні вирази - це шаблони, за допомогою яких програміст може шукати певні текстові образи у великих об'ємах даних. Вони використовуються для ефективного пошуку, валідації та заміни тексту. Використання регулярних виразів допомагає зменшити кількість роботи програміста, оскільки шаблони можна повторно використовувати для різних завдань.

# Як?
```Kotlin
val regex = Regex("[a-z]+")
var text = "abc1234def5678ghi"

val matches = regex.findAll(text)

for (match in matches) {
    println(match.value)
}
```
В результаті виконання коду буде виведено:
```
abc
def
ghi
```

# Глибші води
Регулярні вирази були створені в 1956 році математиком Стівеном Клемом для теорії формальних мов. У сучасних мов програмування, включаючи Kotlin, підтримка регулярних виразів вбудована. Однак, для менших задач, таких як пошук конкретного тексту, можна використати інші методи, наприклад, метод `contains()` для рядків у Kotlin.

# Дивіться також
- Офіційна документація з використання регулярних виразів в Kotlin (https://kotlinlang.org/docs/reference/regular-expressions.html)
- Бібліотека для роботи з регулярними виразами в Kotlin (https://github.com/kotlin-projects/kotlin-regex)