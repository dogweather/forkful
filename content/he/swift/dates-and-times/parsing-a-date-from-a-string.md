---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:00.981600-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\
  \u05D4 \u05E9\u05DC \u05D9\u05D9\u05E6\u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\
  \u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05D5\u05E9\u05E2\u05D4 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\
  \ \u05DE\u05E1\u05D5\u05D2 `Date`. \u05EA\u05D4\u05DC\u05D9\u05DA \u05D6\u05D4 \u05D4\
  \u05DB\u05E8\u05D7\u05D9 \u05D1\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD \u05E9\
  \u05D1\u05D4\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DE\u05EA\u05E7\
  \u05E9\u05E8\u05D9\u05DD \u05DB\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA, \u05DB\
  \u05DE\u05D5 \u05D1\u05EA\u05D2\u05D5\u05D1\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.921629-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05D4\
  \ \u05E9\u05DC \u05D9\u05D9\u05E6\u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\
  \u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D5\u05E9\u05E2\u05D4 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05DE\
  \u05E1\u05D5\u05D2 `Date`."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## מה ולמה?
ניתוח תאריך ממחרוזת כולל המרה של ייצוגים טקסטואליים של תאריך ושעה לאובייקט מסוג `Date`. תהליך זה הכרחי ביישומים שבהם תאריכים מתקשרים כמחרוזות, כמו בתגובות API או קלטים מהמשתמש, מה שמאפשר ניהול ועיצוב תאריכים בקלות רבה יותר.

## איך לעשות:

### באמצעות `DateFormatter` של Foundation
ספריית הסטנדרט של Swift, Foundation, מספקת את `DateFormatter` להמרה של מחרוזות לאובייקטים מסוג `Date` ולהפך. כדי לנתח תאריך ממחרוזת, יש לציין את פורמט התאריך שמתאים למחרוזת, ואז להשתמש במעבד הפורמט לניתוח התאריך.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("ניתחתי תאריך: \(date)")
} else {
    print("נכשלתי בניתוח התאריך")
}
// פלט לדוגמה: ניתחתי תאריך: 2023-04-29 22:00:00 +0000
```

שימו לב שהפלט עשוי להשתנות בהתאם לאזור הזמן שלכם.

### באמצעות ISO8601DateFormatter
עבור פורמטים של תאריכים לפי התקן ISO 8601, Swift מספקת מעבד פורמט מיוחד, `ISO8601DateFormatter`, המפשט את תהליך הניתוח.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("ניתחתי תאריך ISO8601: \(date)")
} else {
    print("נכשלתי בניתוח תאריך ISO8601")
}
// פלט לדוגמה: ניתחתי תאריך ISO8601: 2023-04-30 15:00:00 +0000
```

### שימוש בספרייה חיצונית: SwiftDate
למרות ש-Swift מספקת כלים חזקים לניתוח תאריכים, ספריות חיצוניות כמו SwiftDate מציעות אפשרויות נוספות ונוחות רבה. לאחר הוספת SwiftDate לפרויקט שלכם, הניתוח הופך לפשוט ביותר:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("ניתחתי תאריך עם SwiftDate: \(date)")
} else {
    print("נכשלתי בניתוח תאריך עם SwiftDate")
}
// פלט לדוגמה: ניתחתי תאריך עם SwiftDate: 2023-04-30 00:00:00 +0000
```

SwiftDate מפשטת את הניתוח עם שפה טבעית ומגוון רחב של פורמטי תאריך, והופכת אותה לתוספת חזקה לצרכנה שלכם בתיכנות Swift.
