---
date: 2024-01-20 17:39:41.872228-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05DC\u05D8\
  \ \u05D6\u05D4 \u05DE\u05E8\u05D0\u05D4 \u05D0\u05EA \u05D4\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D0\u05D7\u05E8\u05D9 \u05D4\u05DE\u05E8\u05D4 \u05DC\u05D0\u05D5\
  \u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA."
lastmod: '2024-04-05T21:53:40.941269-06:00'
model: gpt-4-1106-preview
summary: "\u05E4\u05DC\u05D8 \u05D6\u05D4 \u05DE\u05E8\u05D0\u05D4 \u05D0\u05EA \u05D4\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05E8\u05D9 \u05D4\u05DE\u05E8\
  \u05D4 \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  ."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

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
