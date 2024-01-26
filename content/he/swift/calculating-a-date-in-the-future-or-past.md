---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:32:00.546311-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא התהליך שבו אנו מחשבים תאריכים חדשים בהתבסס על תאריך נתון. תכנתים עושים את זה כדי לנהל לוחות זמנים, ליצור תזכורות או לחשב מועדים לאירועים עתידיים.

## איך לעשות:
השתמש ב-`Calendar`, `Date`, ו-`DateComponents` לחישוב תאריכים בקלות:

```Swift
import Foundation

let calendar = Calendar.current
let now = Date()

// חישוב תאריך 3 ימים בעתיד
if let threeDaysLater = calendar.date(byAdding: .day, value: 3, to: now) {
    print("In 3 days: \(threeDaysLater)")
}

// חישוב תאריך 5 שנים אחורה
if let fiveYearsEarlier = calendar.date(byAdding: .year, value: -5, to: now) {
    print("5 years ago: \(fiveYearsEarlier)")
}
```

תצאה אפשרית:
```
In 3 days: 2023-04-03 14:22:11 +0000
5 years ago: 2018-03-31 14:22:11 +0000
```

## ניתוח מעמיק
מאז המצאת המחשבים, חישובי תאריכים היו חיוניים. ב-iOS, `NSDate` היה הכלי הראשון לניהול תאריכים אך עם זמן נוצר `DateComponents`, שהפך למערכת הנבחרת לחישובים מדויקים וקלים להבנה. חלופות כוללות ספריות חיצוניות כמו `SwiftDate`, ולמרות שהן יכולות להיות עשירות בתכונות, הן לא תמיד נחוצות; ה-API הסטנדרטי עושה את העבודה רוב הפעמים.

## ראה גם
- [Apple Documentation for Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Documentation for Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [NSHipster article on NSDateComponents](https://nshipster.com/nsdatecomponents/)
- [SwiftDate - Advanced Date and Time Management in Swift](https://github.com/malcommac/SwiftDate)
