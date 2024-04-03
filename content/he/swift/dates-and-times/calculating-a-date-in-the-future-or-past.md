---
date: 2024-01-20 17:32:00.546311-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\
  \u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\
  \u05D7\u05E9\u05D1\u05D9\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D7\
  \u05D3\u05E9\u05D9\u05DD \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05E0\u05EA\u05D5\u05DF. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05E0\u05D4\u05DC \u05DC\u05D5\u05D7\u05D5\u05EA \u05D6\u05DE\u05E0\u05D9\
  \u05DD, \u05DC\u05D9\u05E6\u05D5\u05E8 \u05EA\u05D6\u05DB\u05D5\u05E8\u05D5\u05EA\
  \ \u05D0\u05D5 \u05DC\u05D7\u05E9\u05D1\u2026"
lastmod: '2024-03-13T22:44:39.927974-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0 \u05D4\
  \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05D7\
  \u05E9\u05D1\u05D9\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D7\u05D3\
  \u05E9\u05D9\u05DD \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05EA\u05D0\
  \u05E8\u05D9\u05DA \u05E0\u05EA\u05D5\u05DF."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

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
