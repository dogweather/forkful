---
date: 2024-01-26 03:47:19.700606-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05D4\u05E7\u05E8\u05D1\u05D4 \u05E9\u05DC\
  \ \u05E2\u05E8\u05DA \u05DE\u05E1\u05E4\u05E8\u05D9 \u05DC\u05D3\u05D9\u05D5\u05E7\
  \ \u05DE\u05E1\u05D5\u05D9\u05DD, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05E1\u05D9\u05E8 \u05E9\u05D1\u05E8\u05D9\u05DD \u05DC\
  \u05D0 \u05E8\u05E6\u05D5\u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\
  \u05E0\u05D4\u05DC \u05D6\u05D9\u05DB\u05E8\u05D5\u05DF, \u05DC\u05E9\u05E4\u05E8\
  \ \u05D0\u05EA \u05D4\u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05D5\u05DC\u05E2\u05DE\
  \u05D5\u05D3 \u05D1\u05D3\u05E8\u05D9\u05E9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.897135-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05E4\
  \u05D9\u05E8\u05D5\u05E9\u05D5 \u05D4\u05E7\u05E8\u05D1\u05D4 \u05E9\u05DC \u05E2\
  \u05E8\u05DA \u05DE\u05E1\u05E4\u05E8\u05D9 \u05DC\u05D3\u05D9\u05D5\u05E7 \u05DE\
  \u05E1\u05D5\u05D9\u05DD, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05E1\u05D9\u05E8 \u05E9\u05D1\u05E8\u05D9\u05DD \u05DC\u05D0\
  \ \u05E8\u05E6\u05D5\u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E0\
  \u05D4\u05DC \u05D6\u05D9\u05DB\u05E8\u05D5\u05DF, \u05DC\u05E9\u05E4\u05E8 \u05D0\
  \u05EA \u05D4\u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05D5\u05DC\u05E2\u05DE\u05D5\
  \u05D3 \u05D1\u05D3\u05E8\u05D9\u05E9\u05D5\u05EA\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## מה ולמה?

עיגול מספרים פירושו הקרבה של ערך מספרי לדיוק מסוים, בדרך כלל כדי להסיר שברים לא רצויים. מתכנתים מעגלים על מנת לנהל זיכרון, לשפר את הקריאות, ולעמוד בדרישות ספציפיות לתחום, כמו הגבלות של מטבע.

## איך לעשות:

Swift מספק מספר דרכים לעיגול מספרים. הנה טעימה:

```Swift
let original = 3.14159

// עיגול סטנדרטי
let standardRounded = round(original) // 3.0

// עיגול למקום עשרוני מסוים
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// עיגול כלפי מטה
let roundedDown = floor(original) // 3.0

// עיגול כלפי מעלה
let roundedUp = ceil(original) // 4.0

print("סטנדרטי: \(standardRounded), עשרוני: \(decimalRounded), למטה: \(roundedDown), למעלה: \(roundedUp)")
```

פלט: `סטנדרטי: 3.0, עשרוני: 3.142, למטה: 3.0, למעלה: 4.0`

## צלילה עמוקה

בהיסטוריה, עיגול הוא מושג מתמטי שקדם למחשבים, חיוני בסחר ובמדע. מסגרת ה-`Foundation` של Swift מציעה פונקציונליות עיגול מקיפה:

- `round(_: )` הוא העיגול החצי-למעלה הטוב והישן.
- `floor(_: )` ו-`ceil(_: )` מטפלים בעיגול כיווני.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` מעניק שליטה דקה יותר עם אנומרציה של חוקי עיגול.

חשוב להיות מודעים לסוג ה-`Decimal` לחישובים פיננסיים מדויקים, שממנע שגיאות שבר עשרוני. כמו כן, כדאי לבדוק את `NSDecimalNumber` לתאימות עם Objective-C.

## ראה גם

- תקן IEEE לחשבון בשבר עשרוני ממשי (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
