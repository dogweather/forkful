---
title:    "Swift: Перетворення рядка на нижній регістр"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні дуже часто зустрічається необхідність конвертування тексту до нижнього регістру. Це може стати корисним, наприклад, для порівняння двох рядків.

## Як

Існує кілька шляхів конвертування рядка до нижнього регістру. Давайте розглянемо кілька прикладів:

```Swift
let name = "ВАСИЛЬ"
print(name.lowercased())
```
Виводом буде `василь`.

За допомогою цього методу ми можемо швидко і просто перетворити будь-який рядок до нижнього регістру.

```Swift
var message = "ПРИВЕТ, мой друг!"
message = message.lowercased()
print(message)
```
Виводом буде `привет, мой друг!`.

## Детальний розбір

Кодування рядка до нижнього регістру є важливим для багатьох завдань. Наприклад, при порівнянні двох рядків, ігноруючи регістр, ми отримаємо більш точне порівняння. Або ж при використанні пошуку тексту, конвертування до нижнього регістру дозволить знайти слово, незалежно від того, який регістр в ньому використовується.

Також варто звернути увагу, що метод `lowercased()` нічого не змінює у вихідному рядку, а повертає новий рядок у нижньому регістрі. Це дозволяє зберегти вихідний рядок для подальшого використання.

## Дивіться також

- [Apple документація про метод `lowercased()`](https://developer.apple.com/documentation/swift/string/2995369-lowercased/)

- [How to Convert a String to Lowercase in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase-in-swift)

- [Converting a String to Lowercase in Swift Tutorial](https://www.ioscreator.com/tutorials/convert-string-to-lowercase-swift)