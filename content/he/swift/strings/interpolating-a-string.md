---
date: 2024-01-20 17:52:23.043758-07:00
description: "\u05E9\u05D9\u05DC\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA (String interpolation) \u05D1-Swift \u05D6\u05D4\
  \ \u05DB\u05E9\u05DE\u05DB\u05E0\u05D9\u05E1\u05D9\u05DD \u05DE\u05E9\u05EA\u05E0\
  \u05D9\u05DD \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\
  \u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D9\u05E9\
  \u05D9\u05E8\u05D5\u05EA. \u05D6\u05D4 \u05DE\u05D5\u05E2\u05D9\u05DC \u05DC\u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D8\u05E7\
  \u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\u05D9\u05DD \u05D1\u05E7\
  \u05DC\u05D5\u05EA\u2026"
lastmod: '2024-03-11T00:14:13.375563-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05D9\u05DC\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA (String interpolation) \u05D1-Swift \u05D6\u05D4 \u05DB\
  \u05E9\u05DE\u05DB\u05E0\u05D9\u05E1\u05D9\u05DD \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD\
  \ \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D9\u05E9\u05D9\u05E8\
  \u05D5\u05EA. \u05D6\u05D4 \u05DE\u05D5\u05E2\u05D9\u05DC \u05DC\u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8\
  \u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\u05D9\u05DD \u05D1\u05E7\u05DC\u05D5\
  \u05EA\u2026"
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
שילוב טקסט במחרוזת (String interpolation) ב-Swift זה כשמכניסים משתנים וביטויים לתוך מחרוזת טקסט ישירות. זה מועיל למתכנתים לכתוב טקסטים דינמיים בקלות ובאופן קריא יותר.

## איך לעשות:
```swift
let name = "דני"
let age = 25
let greeting = "שלום, \(name)! אתה בן \(age) שנים."
print(greeting)
```
פלט דוגמה:
```
שלום, דני! אתה בן 25 שנים.
```

## עומק השקעה
ב-Swift, שילוב מחרוזות התחיל מהגרסה הראשונה והיה חלק מהיתרונות לעומת שפות קודמות. בשפות אחרות כמו C, למשל, צריך היה להשתמש בפונקציות כמו `sprintf` או לרכיב טקסט באמצעות צירוף מחרוזות מסורבל. שילוב מחרוזות ב-Swift מאפשר הכנסת ערכים וביטויים בצורה ישירה וקריאה, וכך משפר את כתיבת הקוד. השילוב נעשה בזמן ריצה של התוכנית, מה שמאפשר גמישות רבה ותגובה לשינויים בזמן אמת.

## ראה גם:
- [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292) במדריך השפה של Swift.
- [String](https://developer.apple.com/documentation/swift/string) documentation ב-Apple Developer.
