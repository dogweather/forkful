---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

Пошук та заміна тексту - це процедура, коли одну частину коду перетворюють на іншу. Програмісти цим користуються для виправлення помилок, оптимізації коду, або модифікації функціональності.

## Як це робити:

Щоб виконати пошук та заміну в Swift, використовуйте `replacingOccurrences(of:with:)`. Перевірте цей приклад:
```Swift
var myStr = "Hello, World!"
myStr = myStr.replacingOccurrences(of: "World", with: "Swift")
print(myStr) // Outputs: "Hello, Swift!"
```

## Пікірський огляд:

Пошук та заміна тексту - основоположний метод в програмуванні, що виник ще з часів ранніх мов програмування. В Swift ви можете використовувати `replacingOccurrences(of:with:)` вже від версії 2.2. Варто зазначити, що цей метод повертає нову стрічку, а оригінал залишається незмінним. Інша альтернатива - регулярні вирази, але вони можуть бути складні для новачків.

## Дивіться також:

Якщо ви хочете докладніше дізнатися про `replacingOccurrences(of:with:)`, подивіться [офіційну документацію Apple](https://developer.apple.com/documentation/swift/string/2893961-replacingoccurrences).
Для повного вивчення роботи з рядками в Swift перегляньте цей [підручник](https://www.hackingwithswift.com/read/0/5).
Також ви можете вивчити використання регулярних виразів в Swift з [цієї статті](https://benscheirman.com/2014/06/regex-in-swift/).