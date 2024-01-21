---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:48.566464-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
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