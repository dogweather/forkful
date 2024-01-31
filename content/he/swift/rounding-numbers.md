---
title:                "עיגול מספרים"
date:                  2024-01-26T03:47:19.700606-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

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
