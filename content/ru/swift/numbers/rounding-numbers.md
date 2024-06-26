---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:05.125610-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Swift \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442\
  \ \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\
  \u043E\u0431\u043E\u0432 \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u044F\
  \ \u0447\u0438\u0441\u0435\u043B. \u0412\u043E\u0442 \u043D\u0435\u043A\u043E\u0442\
  \u043E\u0440\u044B\u0435 \u0438\u0437 \u043D\u0438\u0445."
lastmod: '2024-03-13T22:44:45.674287-06:00'
model: gpt-4-0125-preview
summary: "Swift \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u043D\
  \u0435\u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \u043E\u0432 \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u044F \u0447\
  \u0438\u0441\u0435\u043B."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
Swift предлагает несколько способов округления чисел. Вот некоторые из них:

```Swift
let original = 3.14159

// Стандартное округление
let standardRounded = round(original) // 3.0

// Округление до определенного десятичного знака
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Округление вниз
let roundedDown = floor(original) // 3.0

// Округление вверх
let roundedUp = ceil(original) // 4.0

print("Стандартное: \(standardRounded), Десятичное: \(decimalRounded), Вниз: \(roundedDown), Вверх: \(roundedUp)")
```

Вывод: `Стандартное: 3.0, Десятичное: 3.142, Вниз: 3.0, Вверх: 4.0`

## Подробнее
Исторически округление - это математическое понятие, предшествующее компьютерам, имеющее важное значение в торговле и науке. Фреймворк `Foundation` в Swift предлагает обширные возможности по округлению:

- `round(_: )` используется для стандартного округления по правилу "вверх-вниз".
- `floor(_: )` и `ceil(_: )` применяются для направленного округления.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` предоставляет более точный контроль с использованием перечисления правил округления.

Обращайте внимание на тип `Decimal` для точных финансовых расчетов, который избегает ошибок с плавающей точкой. Также исследуйте `NSDecimalNumber` для совместимости с Objective-C.

## Смотрите также
- Стандарт IEEE для арифметики с плавающей точкой (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
