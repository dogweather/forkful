---
title:                "Переведення рядка в верхній регістр"
html_title:           "Swift: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та чому?

Велика літера - це процес зміни першої букви слова до великої форми. Програмісти це роблять, щоб дотримуватися стилю написання коду або форматування виводу для користувача.

## Як це зробити:

```Swift
let smallLetterString = "привіт, світ"
let capitalLetterString = smallLetterString.capitalized

print(smallLetterString) 
// Вивід: "привіт, світ"

print(capitalLetterString)
// Вивід: "Привіт, Світ"
```

## Поглиблений огляд

В історії програмування, ця можливість була додана в перших версіях мов. У Swift, можна використовувати `uppercaseString` для перетворення всіх букв у рядку на великі, але `capitalized` тільки змінює першу букву кожного слова.

Це ефективний метод, але важливо пам'ятати, що Swift працює із Unicode, тому результат може бути різним від очікуваного при роботі з нестандартними символами.

Метод `capitalized` працює шляхом створення нового рядка, замість того, щоб змінювати існуючий.

## Дивіться також:

- [A Swift Tour](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html) Англійською мовою
- [StackOverflow: Capitalize each word in string Swift?](https://stackoverflow.com/questions/26306326/capitalize-each-word-in-string-swift)