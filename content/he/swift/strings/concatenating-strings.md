---
date: 2024-01-20 17:36:11.622563-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05E7\u05D9\u05D8\u05D5\u05DF \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\
  \u05D5\u05EA \u05E4\u05E2\u05D5\u05DC\u05D4 \u05DC\u05D0 \u05D9\u05E2\u05D9\u05DC\
  \u05D4. \u05D6\u05D4 \u05D1\u05D2\u05DC\u05DC \u05E9\u05D4\u05D9\u05D4 \u05E6\u05E8\
  \u05D9\u05DA \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\
  \u05D8\u05D9\u05DD \u05D7\u05D3\u05E9\u05D9\u05DD \u05D1\u05D6\u05D9\u05DB\u05E8\
  \u05D5\u05DF \u05D1\u05DB\u05DC \u05E4\u05E2\u05DD \u05E9\u05DE\u05D7\u05D1\u05E8\
  \u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA. \u05D1\u05E9\u05E4\u05D5\
  \u05EA \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-04-05T21:53:40.947178-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05E7\u05D9\u05D8\u05D5\u05DF \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\
  \ \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E4\u05E2\u05D5\u05DC\u05D4 \u05DC\u05D0\
  \ \u05D9\u05E2\u05D9\u05DC\u05D4."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
```Swift
let firstName = "יונתן"
let lastName = "כהן"
let fullName = firstName + " " + lastName // שימוש באופרטור חיבור
print(fullName) // פלט: יונתן כהן

// עם interpolation של מחרוזות
let greeting = "שלום, שמי \(firstName) \(lastName)!"
print(greeting) // פלט: שלום, שמי יונתן כהן!

// דרך שלישית עם append
var message = "מר"
message += " "
message += fullName
print(message) // פלט: מר יונתן כהן
```

## צלילה עמוקה
בעבר, קיטון מחרוזות היה לעיתים קרובות פעולה לא יעילה. זה בגלל שהיה צריך ליצור אובייקטים חדשים בזיכרון בכל פעם שמחברים מחרוזות. בשפות מודרניות כמו Swift, קיטון מחרוזות הוא בדרך כלל יעיל יותר, עם מיטובי זיכרון וביצועים שנעשו על ידי מהדר השפה.

כלי חלופיים לקיטון כוללים שימוש במחלקות כמו `NSMutableString` ב-Objective-C, או `StringBuilder` ב-Java, שמועילים במקרים של עיבוד מחרוזות כבד.

ב-Swift, המיטוב נעשה 'תחת המכסה' כך שהתכניתנים יכולים לקטון מחרוזות בצורה אינטואיטיבית, מבלי לחשוש מתוצאות לא יעילות.

## גם כן ראה
- [String Interpolation in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Programming from Scratch](https://www.weheartswift.com/swift-programming-scratch-100-exercises/)
- [Apple's Swift String Documentation](https://developer.apple.com/documentation/swift/string)
