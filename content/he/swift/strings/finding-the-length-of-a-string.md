---
date: 2024-01-20 17:48:16.296840-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05D0\
  \u05EA\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05EA\u05DB\u05D5\u05E0\u05D4\
  \ `count` \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05E7\u05D1\u05DC \u05D0\u05EA \u05D0\u05D5\u05E8\u05DB\u05D4. \u05E0\u05E8\u05D0\
  \u05D4 \u05D0\u05D9\u05DA."
lastmod: '2024-03-13T22:44:39.890950-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Swift, \u05D0\u05EA\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05EA\
  \u05DB\u05D5\u05E0\u05D4 `count` \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D0\u05D5\u05E8\u05DB\
  \u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## איך לעשות:
ב-Swift, אתה משתמש בתכונה `count` של מחרוזת כדי לקבל את אורכה. נראה איך:

```Swift
let greeting = "שלום עולם!"
let length = greeting.count
print("אורך המחרוזת: \(length)")
```

פלט לדוגמה:

```
אורך המחרוזת: 11
```

## צלילה לעומק
בעבר, ב-Swift 1.x, היינו משתמשים ב-'characters.count', אבל מאז Swift 4.0, ספירת התווים הפכה יותר אינטואיטיבית עם `count`. שימו לב ש-Swift תומכת בתווים מתוחכמים (כמו אימוג'ים ותווים משולבים) ולכן הוא סופר אותם כיחידה אחת, גם אם לוקחים יותר מבית אחד. בעוד שיטות חלופיות כמו `utf16.count` או `unicodeScalars.count` עשויות להחזיר ערכים שונים, שכן הן סופרות יחידות קידוד אחרות.

## ראה גם
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [The Swift Programming Language (Strings and Characters)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
