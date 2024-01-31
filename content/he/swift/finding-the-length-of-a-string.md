---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:48:16.296840-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/finding-the-length-of-a-string.md"
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
