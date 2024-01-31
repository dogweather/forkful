---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:17:11.049293-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
השגת התאריך הנוכחי בתכנות היא כשאנחנו מבקשים מהמחשב לתת לנו את היום, החודש והשנה העכשוויים. זה נחוץ לכשרוצים לתייג אירועים בזמן, לוגים, או לחשב פרקי זמן.

## איך לעשות:
```Swift
import Foundation

// קבלת התאריך הנוכחי
let currentDate = Date()

// הדפסת התאריך הנוכחי
print(currentDate)
```
פלט לדוגמה:
```
2023-04-12 07:46:23 +0000
```

כדי לפורמט את התאריך:
```Swift
// פורמטר להצגת תאריך
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .medium
dateFormatter.timeStyle = .medium

// הדפסת תאריך מעוצב
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate)
```
פלט לדוגמה:
```
Apr 12, 2023, 10:46:23 AM
```

## ניתוח עמוק:
הוצאת התאריך הנוכחי היא פעולה פשוטה היום, אבל זה לקח זמן עד שהפכה לסטנדרט בשפות תכנות. בעידן דוס, למשל, היה צורך להתעסק יותר עם החומרה כדי לקבל כזה פרט. אלטרנטיבות כוללות שימוש ב-Calendar API לפרטים מדוקדקים יותר או ספריות זמן חיצוניות כמו SwiftDate.

בשפת Swift, ה-Date() פונקציה מחזירה תאריך ושעה בפורמט UTC. ניתן להשתמש ב-DateFormatter להפיכתו לפורמט אנושי קריא יותר. כדי להתמודד עם בעיות שעון קיץ וחורף, תיקון לאזורי זמן, ועוד, כדאי להבין טוב את ה-API של Foundation המתייחס לטיפול בזמן ותאריכים.

## ראו גם:
- [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [SwiftDate](https://github.com/malcommac/SwiftDate)
