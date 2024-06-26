---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:58.267414-07:00
description: "\u042F\u043A: Swift \u0440\u043E\u0431\u0438\u0442\u044C \u0440\u043E\
  \u0431\u043E\u0442\u0443 \u0437 \u0430\u0441\u043E\u0446\u0456\u0430\u0442\u0438\
  \u0432\u043D\u0438\u043C\u0438 \u043C\u0430\u0441\u0438\u0432\u0430\u043C\u0438\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u044E. \u041E\u0441\u044C \u044F\u043A \u0432\
  \u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u043E\u0433\u043E\u043B\u043E\u0441\
  \u0438\u0442\u0438, \u0434\u043E\u0434\u0430\u0442\u0438, \u0432\u0438\u0434\u0430\
  \u043B\u0438\u0442\u0438 \u0442\u0430 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\
  \u0438 \u0434\u043E\u0441\u0442\u0443\u043F \u0434\u043E \u0435\u043B\u0435\u043C\
  \u0435\u043D\u0442\u0456\u0432 \u0443 \u0441\u043B\u043E\u0432\u043D\u0438\u043A\
  \u0443 Swift."
lastmod: '2024-03-13T22:44:49.907437-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0440\u043E\u0431\u0438\u0442\u044C \u0440\u043E\u0431\u043E\u0442\
  \u0443 \u0437 \u0430\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\
  \u043C\u0438 \u043C\u0430\u0441\u0438\u0432\u0430\u043C\u0438 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u044E."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
weight: 15
---

## Як:
Swift робить роботу з асоціативними масивами простою. Ось як ви можете оголосити, додати, видалити та отримати доступ до елементів у словнику Swift:

```Swift
// Оголошення словника
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Додавання нового елемента
fruitColors["Grape"] = "Purple"

// Отримання значення за його ключем
if let appleColor = fruitColors["Apple"] {
    print("Яблуко \(appleColor).")  // Виведення: Яблуко Red.
} else {
    print("Колір не знайдено.")
}

// Видалення елемента
fruitColors["Banana"] = nil  // Це видалить "Банан" зі словника

// Ітерація по елементах
for (fruit, color) in fruitColors {
    print("\(fruit) є \(color).")
    // Виведення:
    // Яблуко є Red.
    // Виноград є Purple.
}
```

Словники надзвичайно універсальні та дозволяють динамічно керувати та отримувати доступ до даних. Їхня невпорядкованість не впливає на швидкість отримання даних, що є значною перевагою при роботі з великими наборами даних.

## Поглиблено
Реалізація словників у Swift, як асоціативні масиви, ґрунтується на їхній потужній спроможності відображати унікальні ключі на значення. Історично мови програмування реалізовували цей концепт під різними назвами, такими як хеш-таблиці або мапи, які натякають на їхню функціональність створення "мапи" між ключами та значеннями.

У Swift, словники оптимізовані для продуктивності, використовуючи хешовані ключі для ефективного доступу до даних. Це означає, що тип `Key` у словнику `[Key: Value]` повинен відповідати протоколу `Hashable`, що є характерним для більшості стандартних типів Swift, таких як `Int`, `String` та `Double`.

Одна річ, на яку варто звернути увагу, полягає в тому, що хоча словники чудово підходять для асоціації пар даних, вони не мають порядку. Якщо вам потрібно підтримувати порядок елементів, ви можете розглянути альтернативи, такі як `Array` для послідовності впорядкованих елементів або користувацькі структури даних, що поєднують ознаки масивів та словників.

Також варто відзначити, що Swift постійно еволюціонує, і так само робить і його обробка та оптимізація словників. Тому важливо бути в курсі останньої документації Swift, щоб найкраще використовувати словники, переконавшись, що ви використовуєте найбільш ефективні та актуальні практики.
