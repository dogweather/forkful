---
title:                "Перетворення рядка у великі літери."
html_title:           "Swift: Перетворення рядка у великі літери."
simple_title:         "Перетворення рядка у великі літери."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Існує багато сценаріїв, коли потрібно перетворити одиниці тексту у великі або малі літери. Наприклад, при виведенні імен користувачів або при валідації полів форми. Програмістам необхідно знати, як правильно перетворити рядок, щоб досягти вірного результату.

## Як

```Swift
let name = "марія" // рядок, який потрібно перетворити
let capitalizedName = name.capitalized // результат: "Марія"
```

## Глибока невидимість

В регіональних мовах часто буває важко правильно перетворити літери на верхній чи нижній регістри, оскільки існують прописні літери, які складаються з декількох символів. Наприклад, буква "І" українського алфавіту складається з символів "І" та "Ї". У таких випадках потрібно використовувати спеціальні функції, які враховують ці особливості.

## Дивіться також

- [Робота з рядками в Swift](https://medium.com/@inna_matyushyn/%D1%87%D0%B0%D1%81%D1%82%D1%8C-1-%D0%BF%D1%80%D0%B0%D1%86%D1%8E%D1%94%D0%BC%D0%BE-%D0%B7-%D1%80%D1%8F%D0%B4%D0%BA%D0%B0%D0%BC%D0%B8-basic-string-manipulations-ios-swift-c2f2d91fb0c0)
- [Властивісті рядків в Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Інструменти для роботи з рядками в Xcode](https://www.youtube.com/watch?v=Qq_a2nex6eo)