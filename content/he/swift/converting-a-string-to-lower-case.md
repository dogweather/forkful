---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:41.872228-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא פעולה בה אתה משנה את כל התווים במחרוזת לאותיות קטנות. תוכניתנים עושים זאת לצורך אחידות, השוואה בין מחרוזות בלי רגישות לרישיות, ולעיתים להכין טקסט לתצוגה.

## איך לעשות:
```swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hello, world!"
```
פלט זה מראה את המחרוזת אחרי המרה לאותיות קטנות.

## עומק:
המרה לאותיות קטנות הייתה מאפיינת מערכות טקסט עוד מימי הדוס. היא עוזרת במניעת בעיות רגישות לרישיות במהלך השוואת מחרוזות. ב-Swift, `.lowercased()` משתמשת בכללים של הגדרה מקומית (locale) להמרה נכונה של אותיות גדולות, כולל תווים יחודיים משפות שונות. קיימות אלטרנטיבות כמו מעבר ידני על כל תו והמרתו, אבל פעולה זו אינה מומלצת מכיוון שהיא אינה מתחשבת במקרים מיוחדים ומסובכת יותר לתחזוקה.

## ראה גם:
- [מדריך רשמי של Apple לעבודה עם מחרוזות ב-Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [הבנת השפעות הגדרות מקומיות בעבודה עם טקסטים](https://developer.apple.com/documentation/foundation/nslocale)