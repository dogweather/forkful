---
date: 2024-01-20 17:48:16.296840-07:00
description: "\u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\
  \u05D5\u05D0 \u05E1\u05E4\u05D9\u05E8\u05EA \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D1\u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E0\u05E2\
  \u05D6\u05E8\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05EA\
  \u05E7\u05D9\u05E0\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05D2\u05D6\u05D5\u05E8\
  \ \u05DE\u05D9\u05D3\u05E2 \u05D5\u05DC\u05E0\u05D4\u05DC \u05EA\u05D4\u05DC\u05D9\
  \u05DB\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA."
lastmod: '2024-03-11T00:14:13.383912-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D5\
  \u05D0 \u05E1\u05E4\u05D9\u05E8\u05EA \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E0\u05E2\u05D6\
  \u05E8\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05EA\u05E7\
  \u05D9\u05E0\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\u05D2\u05D6\u05D5\u05E8 \u05DE\
  \u05D9\u05D3\u05E2 \u05D5\u05DC\u05E0\u05D4\u05DC \u05EA\u05D4\u05DC\u05D9\u05DB\
  \u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
אורך מחרוזת הוא ספירת התווים בה. תוכניתנים נעזרים בזה לוודא תקינות קלט, לגזור מידע ולנהל תהליכים שונים במחרוזות.

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
