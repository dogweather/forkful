---
title:                "Рефакторинг"
date:                  2024-01-26T01:47:25.137341-07:00
model:                 gpt-4-0125-preview
simple_title:         "Рефакторинг"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## Що та чому?
Рефакторинг - це процес модифікації існуючого коду для покращення його структури, читабельності та продуктивності без зміни зовнішньої поведінки. Програмісти проводять рефакторинг, щоб зробити код легшим для підтримки, спростити додавання нових функцій, а також легше знаходити та виправляти помилки.

## Як:
Ось фрагмент коду на Kotlin, який демонструє поширену проблему коду та його відрефакторену версію. Ми починаємо з частини коду, яка виконує занадто багато дій:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Розрахунок загальної суми замовлення
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Застосування знижки
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Далі обробка...
    }
}
```

Рефакторинг для кращої читабельності та розділення відповідальності:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Тут немає прикладу виводу, оскільки ми не змінили функціонал, але читабельність і легкість підтримки коду значно покращилися!

## Поглиблений огляд
Рефакторинг як концепція існує з моменту початку програмування, але він справді набув популярності як дисципліна у 1990-х роках, особливо після публікації Мартіна Фаулера "Рефакторинг: Поліпшення дизайну існуючого коду" у 1999 році. Ця книга дала назву практиці та визначила організований метод її застосування, включно з каталогом технік рефакторингу.

Порівнюючи рефакторинг з альтернативами: ви можете переписувати код з нуля (ризиковано та часозатратно) або просто робити додаткові зміни (призводить до збільшення розміру програмного забезпечення та потенційного технічного боргу). Рефакторинг займає золоту середину - він оновлює та очищає, зберігаючи низький ризик.

Що стосується впровадження, то важливо мати надійний набір тестів перед початком рефакторингу, щоб переконатися, що ви випадково не зміните поведінку програми. Багато сучасних IDE (включаючи IntelliJ для Kotlin) мають автоматизовані інструменти рефакторингу для перейменування змінних, виділення методів та іншого, що може прискорити процес і зменшити кількість помилок.

## Дивіться також
- "Рефакторинг: Поліпшення дизайну існуючого коду" Мартіна Фаулера (за основну працю з цієї теми)
- Документація Kotlin про конвенції кодування: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (щоб зрозуміти "шлях Kotlin" до чистого коду)
- Підтримка рефакторингу в IntelliJ IDEA від JetBrains: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (для практичного використання інструментів рефакторингу)
- Посібник Google з рефакторингу на велику шкалу: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (для розуміння, як вирішити більші виклики рефакторингу)
