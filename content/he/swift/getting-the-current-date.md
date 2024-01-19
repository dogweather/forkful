---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

לקבל את התאריך הנוכחי הוא בסופו של דבר להביא את הזמן מהעכשיו, מהשנייה הזו. תוכניתים מנצלים את זה למגוון מטרות כמו ניהול אירועים, עדכונים, תקופות עבודה ותזכורות.

## איך?

קבלת התאריך הנוכחי בשפת התכנות Swift מתבצעת דרך קלאס נוסף של המערכת, `Date`. הנה דוגמה:

```Swift
let currentDate = Date()
print(currentDate)
```

הפלט יהיה ממד כזה:

```Swift
2022-03-01 13:20:45 +0000
```

## הצצה לעומק

אף פעם לא הייתה אחת שמה דרך היכולה להוציא את התאריך והשעה הנוכחיים עד להופעת הפקודה `Date()` בשפת Swift. על אף השילוב הנוח שלה, ישנם דרכים נוספות לקבלת התאריך, כמו למשל יצירת מופע של `NSDate` .

Swift משתמשת ב-Time Interval או בגרגוריאני כדי לעבוד עם תאריכים. זו עובדת על ידי שמירה על מספר שניות שעברו מאז מועד כלשהו (כמו תחילת המאה ה-21). מספרי השניות הממוזגים הם בפורמט fractional, מה שמאפשר להם לחשב יותר משנייה אחת.

## לעיון נוסף

1. [הדרכה מקיפה לתאריך ושעה ב-Swift](https://www.hackingwithswift.com/articles/141/8-important-swift-dates)
2. [תיעוד ה-Date الرسمي](https://developer.apple.com/documentation/foundation/date)
3. [צפון ה-NSDate](https://developer.apple.com/documentation/foundation/nsdate)