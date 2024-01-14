---
title:    "Swift: השוואת שתי תאריכים"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה:
יש כמה סיבות שיכולות לגרום למדד את התאימות בין שתי תאריכים. למשל, אם אתה עובד עם אפליקציה שמנה ימים מאז סיום של פרוייקט או מתאם בין תאריכי טסים במערכת המכירות שלך. כתוצאה מכך, יש צורך ללמוד כיצד להשוות שתי תאריכים בשפת סוויפט.

## איך לעשות:
כדי להשוות שני תאריכים בשפת סוויפט, ניתן להשתמש בפעולת השוואה של הפרוטוקול Date. למשל, כדי להשוות את התאריך הנוכחי לתאריך מבוקש, ניתן להשתמש בפעולה "isEquivalent(to:)" כדי לבדוק אם שני התאריכים זהים. ניתן גם להשתמש במגוון פעולות נוספות כגון "isBefore(_ other: Date)" ו-"isAfter(_ other: Date)" כדי לבדוק את היחס בין שני התאריכים.

```Swift
let currentDate = Date() // יצירת תאריך נוכחי
let dateToCompare = Date(timeIntervalSinceNow: 86400) // יצירת תאריך מחר

currentDate.isEquivalent(to: dateToCompare) // פלט: false
currentDate.isBefore(dateToCompare) // פלט: true
currentDate.isAfter(dateToCompare) // פלט: false
```

ייתכן גם שתרצה להשתמש בפעולת השוואה בין שני תאריכים שונים בפורמט של תאריך ושעה. במקרה כזה, יהיה נחוץ להשתמש במגוון פעולות נוספות כגון "compare(_:)" כדי לקבוע את היחס בין התאריכים.

```Swift
let date1 = DateComponents(year: 2019, month: 9, day: 1, hour: 12).date! // תאריך שנתן יצירה: 1 בספטמבר 2019, 12:00
let date2 = DateComponents(year: 2020, month: 1, day: 1, hour: 12).date! // תאריך שנתן יצירה: 1 בינואר 2020, 12:00

date1.compare(date2) // פלט: תאריך 1 משנה לפני תאריך 2 (date1 < date2)
date2.compare(date1) // פלט: תאריך 1 משנה אחרי תאריך 2 (