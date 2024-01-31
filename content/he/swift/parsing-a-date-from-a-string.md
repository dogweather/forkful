---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:38:48.510156-07:00
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
לנתח תאריך ממחרוזת זה להמיר מהות טקסט (מחרוזת) לפורמט התאריך הגיוני שהמחשב יכול להבין ולעבוד איתו. תכנתים עושים זאת כי רוצים להשוות תאריכים, לחשב הפרשים ביניהם, או לפורמטם להצגה.

## איך לעשות:
ב-Swift, נייצר אובייקט `DateFormatter` ונגדיר את פורמט התאריך. לדוגמא:

```Swift
import Foundation

let dateString = "23-02-2023 17:45"

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy HH:mm"
dateFormatter.locale = Locale(identifier: "he_IL") // לקוח בחשבון שמירה על פורמט התאריך עברי

if let date = dateFormatter.date(from: dateString) {
    print("התאריך: \(date)")
} else {
    print("לא הצלחנו לנתח את המחרוזת לתאריך.")
}

// Output:
// התאריך: 2023-02-23 15:45:00 +0000
```
## טבילה עמוקה:
לנתח תאריכים זה אתגר ישן כמו התכנות עצמו. לפני ש-Swift הכיר ב-`DateFormatter`, היו נפוצים גישות רודניות יותר, כמו ניתוח ידני של מחרוזות. כיום, יש גם אלטרנטיבות כמו הספרייה `SwiftDate` שמקלה על הניתוח ומציעה פונקציונליות נוספת. עם זאת, הבנה של `DateFormatter` חשובה עדין כי היא מספקת גמישות ודיוק. שימו לב לזמן המקומי ולמיקום, `locale`, שיכולים לשפיע על הנתוח.

## גם כדאי לראות:
- [NSDateFormatter - Apple Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [SwiftDate Library on GitHub](https://github.com/malcommac/SwiftDate)
- [Locale - Apple Documentation](https://developer.apple.com/documentation/foundation/locale)
