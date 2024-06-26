---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:01.818861-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u043A\u0430\u0440\
  \u0442\u044B \u0432 Kotlin \u043F\u0440\u043E\u0441\u0442\u043E. \u0412\u043E\u0442\
  \ \u043A\u0440\u0430\u0442\u043A\u043E\u0435 \u0440\u0443\u043A\u043E\u0432\u043E\
  \u0434\u0441\u0442\u0432\u043E, \u043A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\
  \u0435\u043B\u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:44.964938-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u043A\u0430\u0440\u0442\
  \u044B \u0432 Kotlin \u043F\u0440\u043E\u0441\u0442\u043E."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0430\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445\
  \ \u043C\u0430\u0441\u0441\u0438\u0432\u043E\u0432"
weight: 15
---

## Как это сделать:
Создание и использование карты в Kotlin просто. Вот краткое руководство, как это сделать:

```Kotlin
fun main() {
    // Создание изменяемой карты
    val fruits = mutableMapOf("a" to "Apple", "b" to "Banana")

    // Добавление элементов
    fruits["o"] = "Orange" // Используя операцию индексации
    fruits.put("g", "Grape") // Используя метод put

    // Доступ к элементам
    println(fruits["a"])  // Вывод: Apple
    println(fruits["b"])  // Вывод: Banana

    // Удаление элементов
    fruits.remove("b")
    
    // Перебор карты
    for ((key, value) in fruits) {
        println("$key -> $value")
    }
    // Пример вывода:
    // a -> Apple
    // o -> Orange
    // g -> Grape
}
```

## Подробнее
Карты Kotlin напрямую берут начало из его взаимодействия с Java, где карты являются неотъемлемой частью коллекций. Однако Kotlin повышает их удобство использования, предлагая как изменяемые (`MutableMap`), так и только для чтения (`Map`) интерфейсы, в отличие от единого интерфейса `Map` в Java. Это различие делает ясным, предназначена ли коллекция для модификации или нет.

Значительная деталь реализации карт в Kotlin - явное различие между изменяемыми и неизменяемыми картами, что подчеркивает акцент языка на неизменяемости и безопасности потоков.

Хотя карты очень полезны, Kotlin также предлагает другие коллекции, такие как списки и множества, каждая из которых имеет свое назначение. Например, списки сохраняют порядок и позволяют дублирование, делая их идеальными для доступа к элементам по индексу, в то время как множества обеспечивают уникальность, но не поддерживают порядок. Выбор между использованием карты, списка или множества зависит от конкретных требований вашего приложения, например, от необходимости доступа на основе ключа или сохранения порядка.

С точки зрения лучших альтернатив, если производительность критична, особенно с большими коллекциями, рассмотрите возможность использования специализированных, более эффективных структур данных, предоставляемых внешними библиотеками, которые оптимизированы для конкретных случаев использования, таких как одновременный доступ или сортировка.
