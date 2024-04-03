---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:40.172974-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DE\u05E1\u05D2\u05E8\u05EA \u05D4-`Foundation` \u05E9\u05DC Swift \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D0\u05EA \u05DE\u05D7\u05DC\u05E7\u05EA \u05D4-`Date`, \u05D4\
  \u05D4\u05D5\u05E4\u05DB\u05EA \u05D0\u05EA \u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\u05DB\
  \u05D7\u05D9\u05D9\u05DD \u05DC\u05E4\u05E9\u05D5\u05D8\u05D4. \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D0 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05E7\
  \u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\
  \u05D7\u05D9."
lastmod: '2024-03-13T22:44:39.923509-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05D2\u05E8\u05EA \u05D4-`Foundation` \u05E9\u05DC Swift \u05DE\
  \u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05DE\u05D7\u05DC\u05E7\u05EA \u05D4-`Date`,\
  \ \u05D4\u05D4\u05D5\u05E4\u05DB\u05EA \u05D0\u05EA \u05E7\u05D1\u05DC\u05EA \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\
  \u05DB\u05D7\u05D9\u05D9\u05DD \u05DC\u05E4\u05E9\u05D5\u05D8\u05D4."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות זאת:
מסגרת ה-`Foundation` של Swift מספקת את מחלקת ה-`Date`, ההופכת את קבלת התאריך והשעה הנוכחיים לפשוטה. הנה דוגמא בסיסית לקבלת התאריך הנוכחי:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

זה יפיק משהו כמו:

```
2023-04-12 07:46:23 +0000
```

תבנית הפלט עוקבת אחרי התקן ISO 8601, בשימוש באזור הזמן UTC. עם זאת, ייתכן שתרצה לעצב את התאריך הזה לצורכי הצגה. מחלקת ה-`DateFormatter` של Swift מגיעה להצלה:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

דוגמת פלט יכולה להיות:

```
12 באפריל 2023 בשעה 10:46:23
```

שים לב שתבנית הפלט תשתנה בהתאם לאזור המקומי של המכשיר שמריץ את הקוד.

עבור פרויקטים שדורשים טיפול מורכב יותר בתאריכים, רבים ממפתחי Swift פונים לספריות צד שלישי כמו `SwiftDate`. הנה איך אפשר להשתמש ב-`SwiftDate` כדי לקבל את התאריך הנוכחי באזור זמן ובתבנית מסוימים:

ראשית, הוסף את `SwiftDate` לפרויקט שלך באמצעות SPM, CocoaPods, או Carthage. לאחר מכן:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

זה עשוי להפיק:

```
2023-04-12 09:46:23
```

בשימוש ב-`SwiftDate`, אתה יכול בקלות למניפולציה תאריכים ושעות עבור אזורי זמן ואזורים מקומיים שונים, מה שמפשט משימות מורכבות של טיפול בתאריכים באפליקציות Swift שלך.
