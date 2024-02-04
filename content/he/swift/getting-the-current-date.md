---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:11:40.172974-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-Swift מתבצעת על ידי שימוש במחלקת `Date` כדי לגשת לתאריך ולשעה שבהם רץ האפליקציה. מתכנתים צריכים לאחזר את התאריך הנוכחי ממגוון סיבות הנעות מתיוג זמני של אירועים, ביצוע חישובי תאריכים, ועד הצגת תאריכים ושעות בממשק המשתמש.

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
