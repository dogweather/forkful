---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Kotlin: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому? 

Розрахунок дати у майбутньому або минулому - це процес, за допомогою якого програмісти можуть знаходити потрібну дату, додавши або віднявши певну кількість часу від поточної дати. Це корисно для створення інтерактивних календарів, а також для автоматичної генерації дат для подій або розкладів.

## Як це зробити: 

```Kotlin
// Щоб додати кількість днів до поточної дати:
val currentDate = LocalDate.now()
val futureDate = currentDate.plusDays(30)
println(futureDate) // Виведе дату, яка буде через 30 днів

// Щоб відняти кількість років від певної дати:
val givenDate = LocalDate.of(1995, Month.JULY, 8)
val pastDate = givenDate.minusYears(25)
println(pastDate) // Виведе дату, яка була 25 років тому від певної дати
```

## Вглиб: 

Історичний контекст розрахунку дат зв'язаний з виникненням календаря, який був розроблений людством для визначення поняття часу. Існує також кілька альтернативних підходів до розрахунку дат, наприклад, за допомогою бібліотеки Java.time в мові Java або за допомогою функцій у JavaScript. 

Якщо йдеться про реалізацію розрахунку дат в майбутньому або минулому в коді, важливо враховувати різницю між часовими зонами та локаціями, а також правила переходу на зимовий та літній час. Також слід уникати проблем з точністю при використанні наївних підходів до розрахунку дат. 

## Дивіться також: 

- Інформація про бібліотеку Java.time в мові Java: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Альтернативний підхід до розрахунку дат у JavaScript: [https://momentjs.com/](https://momentjs.com/)
- Детальніше про правила переходу на зимовий та літній час: [https://uk.wikipedia.org/wiki/Переходу_на_летній_час](https://uk.wikipedia.org/wiki/Переходу_на_летній_час)