---
date: 2024-01-20 17:33:48.566464-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Swift \u05DE\u05E1\
  \u05E4\u05E7 \u05D0\u05EA `Date` \u05DC\u05E1\u05D5\u05D2 \u05E9\u05E2\u05D5\u05DF\
  \ \u05D5\u05D0\u05EA `Calendar` \u05DC\u05DE\u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\
  \u05D9\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD. \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05D1\u05D3\u05D5\u05E7."
lastmod: '2024-03-13T22:44:39.926442-06:00'
model: gpt-4-1106-preview
summary: "Swift \u05DE\u05E1\u05E4\u05E7 \u05D0\u05EA `Date` \u05DC\u05E1\u05D5\u05D2\
  \ \u05E9\u05E2\u05D5\u05DF \u05D5\u05D0\u05EA `Calendar` \u05DC\u05DE\u05E0\u05D9\
  \u05E4\u05D5\u05DC\u05E6\u05D9\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  ."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
Swift מספק את `Date` לסוג שעון ואת `Calendar` למניפולציות תאריכים. בואו נבדוק:

```swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd HH:mm"

let date1 = formatter.date(from: "2023/04/01 09:00")!
let date2 = formatter.date(from: "2023/04/01 17:00")!

// השוואת התאריכים
if date1 < date2 {
    print("date1 is earlier than date2")
} else if date1 > date2 {
    print("date1 is later than date2")
} else {
    print("The dates are the same")
}
```

תוצאת דוגמה:
```
date1 is earlier than date2
```

## צלילה לעומק:
המחלקה `Date` בSwift היא לא חדשה, מגיעה מObjective-C. ניתן להשוות בין `Date` באמצעות אופרטורים סטנדרטיים כמו `<` ו `>`. יש גם פונקציות ב`Calendar` לחישוב הפרשים. כמו כן, יש ספריות חיצוניות כמו SwiftDate, אך לרוב עדיף להישאר עם המובנות.

## ראו גם:
- [Documentation on Date - Apple Developer](https://developer.apple.com/documentation/foundation/date)
- [SwiftDate, a powerful Dates and Times handling library](https://github.com/malcommac/SwiftDate)
