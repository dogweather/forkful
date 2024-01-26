---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:39:20.706021-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення рядків на нижній регістр - це зміна усіх великих літер на маленькі. Це корисне для уніфікації тексту, наприклад, під час порівняння або пошуку даних.

## Як це зробити:
```Swift
let originalString = "Привіт, Світ!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)  // Виводить: "привіт, світ!"
```

## Докладно:
Історично методи перетворення рядків на нижній регістр виникли з необхідності обробки тексту в однорідний спосіб. На відміну від методу `.lowercased()`, є метод `.uppercaseString`, який робить протилежне. Перетворення в Swift враховує локалізацію: наприклад, "i" до "I" в Турецькій локалі стане "İ". Також слід зазначити, що `.lowercased()` використовує Unicode для правильного відображення символів.

## Див. також:
- [String Documentation on Apple’s Developer Website](https://developer.apple.com/documentation/swift/string)
- [Unicode Standard on Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Swift API Design Guidelines](https://swift.org/documentation/api-design-guidelines/)
