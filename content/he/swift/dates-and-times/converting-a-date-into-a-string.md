---
date: 2024-01-20 17:37:49.239434-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05E0\
  \u05E2\u05E9\u05D4 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1-`DateFormatter` \u05DC\u05D4\
  \u05DE\u05E8\u05D4 \u05D6\u05D5. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4\
  ."
lastmod: '2024-03-13T22:44:39.925055-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Swift, \u05E0\u05E2\u05E9\u05D4 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\
  -`DateFormatter` \u05DC\u05D4\u05DE\u05E8\u05D4 \u05D6\u05D5."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך לעשות:
ב-Swift, נעשה שימוש ב-`DateFormatter` להמרה זו. הנה דוגמה:

```swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let date = Date()
let dateString = dateFormatter.string(from: date)
print(dateString)
```

פלט דוגמה:

```
23/04/2023
```

אם נרצה פורמט שונה, פשוט נשנה את `dateFormat`:

```swift
dateFormatter.dateFormat = "E, d MMM yyyy HH:mm:ss"
let dateStringWithTime = dateFormatter.string(from: date)
print(dateStringWithTime)
```

פלט דוגמה:

```
Sun, 23 Apr 2023 14:36:52
```

## ניתוח עמוק
פורמטר תאריכים ב-Swift התפתח מהסביבות הראשונות של Objective-C. כמעט כל מה שנעשה כיום ב-Swift עם `DateFormatter` ניתן היה לבצע באופן דומה ב-Objective-C.

חלופות ל-`DateFormatter` כוללות שימוש ב-Swift 5.5 ומעלה ב-`ISO8601DateFormatter` לפורמט בסטנדרט ISO 8601, או `RelativeDateTimeFormatter` להצגת תאריכים יחסיים כמו "לפני 3 שעות".

בביצוע המרה, שימו לב לביצועים: `DateFormatter` יכול להיות משאב רב בשימוש חוזר; כדי למנוע זאת, אפשר לאתחל אותו פעם אחת ולשנות את ה-`dateFormat` לפי הצורך.

## ראו גם
- המדריך המלא ל-`DateFormatter` בקישור הזה: [Date Formatting Guide](https://developer.apple.com/documentation/foundation/dateformatter)
- מסמכי אפל לעבודה עם תאריכים וזמנים: [Working with Dates and Times](https://developer.apple.com/documentation/foundation/dates_and_times)
- שאלות נפוצות על מיפוי התאריכים למחרוזות ב-Stack Overflow: [Date to String in Swift](https://stackoverflow.com/questions/tagged/swift+date+string)
