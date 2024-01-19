---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

---

## Що це та навіщо це потрібно?

Видобування підрядків - це процес вибору специфічного біта інформації з масиву символів, що відомий як рядок. Програмісти це роблять, щоб розділити і перероблять великі шматки даних на керовані, зрозумілі частини.

## Як це зробити:

Якщо у вас є рядок у Swift, ви можете обрати підрядок за допомогою індексів. Ось як це може виглядати:

```Swift
let str = "Hello, World!"
let indexStartOfText = str.index(str.startIndex, offsetBy: 7)
let indexEndOfText = str.index(str.endIndex, offsetBy: -1)
let substring1 = str[indexStartOfText..<indexEndOfText]
print(substring1)  // prints "World"
```

## Поглиблений взгляд:

Історично, відобуття підрядків є фундаментальною операцією в обробці тексту в усіх мовах програмування. У Swift, підрядки не зберігають власний масив символів, замість цього вони зберігають посилання на початковий рядок та область, яку вони представляють, це є більш ефективним за рахунок пам'яті.

## Дивіться також:

Приступаючи до видобування підрядків у Swift, можливо, вам захочеться більше дізнатися про інші операції з рядками:

- [Вивчення рядків в Swift](https://learnswift.tiye.me/strings/): Комплексний посібник по рядкам у Swift.
- [Документація Apple по Swift Strings](https://developer.apple.com/documentation/swift/string): Офіційна документація Apple, яка дає загальне введення в роботу з рядками в Swift. 

---