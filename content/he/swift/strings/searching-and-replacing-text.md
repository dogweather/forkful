---
date: 2024-01-20 17:59:03.224716-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E9\u05D9\u05DE\
  \u05D5 \u05DC\u05D1 \u05E9\u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1-`replacingOccurrences(of:with:)`\
  \ \u05DC\u05D0 \u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA \u05D4\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D4\u05DE\u05E7\u05D5\u05E8\u05D9\u05EA, \u05D0\u05DC\u05D0 \u05D9\
  \u05D5\u05E6\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D7\u05D3\u05E9\u05D4\
  ."
lastmod: '2024-04-05T21:53:40.939625-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05D9\u05DE\u05D5 \u05DC\u05D1 \u05E9\u05D4\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05D1-`replacingOccurrences(of:with:)` \u05DC\u05D0 \u05DE\u05E9\u05E0\u05D4\
  \ \u05D0\u05EA \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05DE\u05E7\u05D5\
  \u05E8\u05D9\u05EA, \u05D0\u05DC\u05D0 \u05D9\u05D5\u05E6\u05E8 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D7\u05D3\u05E9\u05D4."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## איך לעשות:
```Swift
let originalText = "זה טקסט לדוגמא. דוגמא טובה."
let searchText = "דוגמא"
let replacementText = "דוגמה"

let replacedText = originalText.replacingOccurrences(of: searchText, with: replacementText)
print(replacedText)  // "זה טקסט לדוגמה. דוגמה טובה."
```
שימו לב שהשימוש ב-`replacingOccurrences(of:with:)` לא משנה את המחרוזת המקורית, אלא יוצר מחרוזת חדשה.

## צלילה עמוקה
בשנים הראשונות של מחשבים, עיבוד טקסט היה משימה עקרית. עם קוד אסמבלי ושפות תכנות מוקדמות, החלפות נעשו בתהליכים קשים יותר. כיום ב-Swift, פונקציות מובנות כמו `replacingOccurrences(of:with:)` מקלות על התהליך. ישנם גם דרכים אחרות, כמו פעולת חתך ותפר (splice and dice) או שימוש בנוסחאות רגולריות (regular expressions) עבור החלפות מורכבות יותר. הבחירה במימוש תלויה במקרה השימוש שלכם.

## ראו גם
- [Documentation for Swift's String](https://developer.apple.com/documentation/swift/string)
- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression) for more complex search and replace actions.
