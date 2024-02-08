---
title:                "המרת תאריך למחרוזת"
aliases:
- he/swift/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:49.239434-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא פעולה שמשנה את פורמט התאריך לטקסט. מתכנתים מבצעים זאת כדי להציג תאריכים בפורמט הנקרא והמובן למשתמשים.

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
