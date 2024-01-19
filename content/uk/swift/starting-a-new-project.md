---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Створення нового проекту в Swift: Що й для чого?

## Що й для чого?
Створення нового проекту – це створення бази для вашого додатку чи програми. Ми робимо це, щоб мати чітку структуру, із оточенням та файлами, які будуть підтримувати наш код.

## Як це зробити:
Для створення нового проекту в Swift необхідно виконати декілька простих кроків. Демонстрація наведена нижче:

```Swift
// Відкриваємо Xcode
// Натискаємо: File -> New -> Project

// Вибираємо template для нашого проекту. Припустимо, "Single View App".
// Клацніть на кнопку "Next"

// Заповнюємо деталі проекту. Наприклад:
// -Product Name: 'MyApp'
// -Team: '(Ваша команда)'
// -Organization Name: '(Назва вашої організації)'
// -Organization Identifier: '(Ідентифікатор вашої організації)'
// -Language: 'Swift'
// -Interface: 'Storyboard'
// -Lifecycle: 'UIKit App Delegate'
// -Check 'Use Core Data' та 'Include Unit Tests' чи 'Include UI Tests', якщо ви плануєте їх використовувати в проекті.
```

## Поглиблений розгляд
**В ретроспективі**
Swift була розроблена Apple у 2014 році як більш проста і безпечна альтернатива Objective-C. Xcode's "new project" wizard значно спрощує початкову конфігурацію нового проекту.

**Альтернативи**
Багато розробників зараз використовують SwiftUI замість UIKit, який є більш просунутим та декларативним інструментом для створення UI.

**Реалізаційні деталі**
Створюючи новий проект, Xcode створює скелет аплікації з основними класами, такими як AppDelegate і ViewController. 

## Дивіться також
- Керівництво Apple з розробки додатків на Swift: https://developer.apple.com/swift/
- Офіційний документація Swift: https://docs.swift.org/swift-book/
- Уроки з програмування на Swift від Apple:  https://developer.apple.com/tutorials/app-dev-training