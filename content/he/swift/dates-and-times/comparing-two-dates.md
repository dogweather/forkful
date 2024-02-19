---
aliases:
- /he/swift/comparing-two-dates/
date: 2024-01-20 17:33:48.566464-07:00
description: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05D1\u05D9\u05DF \u05E9\u05EA\
  \u05D9 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05DC\u05D1\u05D3\
  \u05D5\u05E7 \u05D0\u05D9\u05DC\u05D5 \u05DE\u05D4\u05DD \u05E7\u05D5\u05D3\u05DD\
  \ \u05D0\u05D5 \u05D0\u05DD \u05D4\u05DD \u05E9\u05D5\u05D5\u05D9\u05DD. \u05EA\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\
  \u05E0\u05D4\u05DC \u05EA\u05D6\u05DB\u05D5\u05E8\u05D5\u05EA \u05D0\u05D5 \u05DC\
  \u05D7\u05E9\u05D1 \u05E4\u05E8\u05E7\u05D9 \u05D6\u05DE\u05DF."
lastmod: 2024-02-18 23:08:53.216625
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05D1\u05D9\u05DF \u05E9\u05EA\u05D9\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D6\u05D4 \u05DC\u05D1\u05D3\u05D5\
  \u05E7 \u05D0\u05D9\u05DC\u05D5 \u05DE\u05D4\u05DD \u05E7\u05D5\u05D3\u05DD \u05D0\
  \u05D5 \u05D0\u05DD \u05D4\u05DD \u05E9\u05D5\u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DC\u05E1\u05D3\u05E8 \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\u05E0\
  \u05D4\u05DC \u05EA\u05D6\u05DB\u05D5\u05E8\u05D5\u05EA \u05D0\u05D5 \u05DC\u05D7\
  \u05E9\u05D1 \u05E4\u05E8\u05E7\u05D9 \u05D6\u05DE\u05DF."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
להשוות בין שתי תאריכים זה לבדוק אילו מהם קודם או אם הם שווים. תכניתנים עושים זאת לסדר אירועים, לנהל תזכורות או לחשב פרקי זמן.

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
