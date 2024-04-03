---
date: 2024-01-20 17:46:33.697880-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D4\u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\
  \u05D5 \u05D0\u05EA\u05DD \u05DC\u05D5\u05E7\u05D7\u05D9\u05DD \u05D7\u05EA\u05D9\
  \u05DB\u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\
  \u05D9\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05E8\u05D5\
  \u05D0, \u05DC\u05E2\u05D1\u05D3, \u05D0\u05D5 \u05DC\u05E0\u05EA\u05D7 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05D1\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA."
lastmod: '2024-03-13T22:44:39.887838-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D4\u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5\
  \ \u05D0\u05EA\u05DD \u05DC\u05D5\u05E7\u05D7\u05D9\u05DD \u05D7\u05EA\u05D9\u05DB\
  \u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\
  \u05D9\u05DE\u05EA."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## מה ולמה?
חילוץ תת-מחרוזות הוא תהליך שבו אתם לוקחים חתיכה מתוך מחרוזת קיימת. תכנתים עושים את זה כדי לקרוא, לעבד, או לנתח נתונים מסוימים במחרוזת.

## איך לעשות:
Swift מספקת כמה דרכים לחלץ תת-מחרוזות. הנה דוגמה:

```Swift
let fullString = "שלום, עולם!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 6)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 10)
let substring = fullString[startIndex...endIndex]

print(substring) // תדפיס "עולם"
```

או באמצעות `Range`:

```Swift
let range = fullString.range(of: "עולם")!
let substring = fullString[range]

print(substring) // תדפיס "עולם"
```

## צלילה לעומק:
בעבר, בגרסאות קודמות של Swift, אנשים נעזרו במתודות NSString מ-Objective-C לחילוץ מחרוזות. עם הזמן, Swift הפכה ליעילה ומאפשרת עבודה ישירה עם מחרוזות באמצעות סינטקס מובנה שלה. השימוש בתת-מחרוזות ב-Swift מחזיר `Substring`, שהוא טיפוס שמשתמש בחלק מזיכרון המחרוזת המקורית ולכן הוא יעיל יותר מאשר ליצור מחרוזת חדשה.

## ראה גם:
- [דוקומנטציה הרשמית של Swift למחרוזות ותת-מחרוזות](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [מדריך ל-Ranges ב-Swift](https://www.avanderlee.com/swift/ranges-explained/)
