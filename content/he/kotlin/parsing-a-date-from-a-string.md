---
title:                "המרת תאריך מתוך מחרוזת"
html_title:           "Kotlin: המרת תאריך מתוך מחרוזת"
simple_title:         "המרת תאריך מתוך מחרוזת"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

מה ולמה?

להמית תאריך ממחרוזת הוא פעולה נפוצה בתכנות שמאפשרת למפענחים לקרוא ולהתייחס לתאריך כדי לעבד אותו כערך ממוחשב. זהו דרך נוחה להתאים את המידע הזמני לפורמט הרצוי ולבצע פעולות מתאימות על התאריך. מתכנתים מבצעים זאת כדי לעבד נתונים כגון תאריכי עליונות גישה באתרים או להציג תאריכים בצורה שאינם נתמכים במערכת הפעלה שלהם.

כיצד לבצע זאת:

Dזה נעשה פשוט על ידי קבע פורמט התאריך הרצוי ולהמית את התאריך מהמחרוזת באמצעות פונקציית בנאי תאריך ב-Kotlin. הנה דוגמא לקוד שיכול לבצע זאת:

```
val dateFormat = SimpleDateFormat("MM/dd/yyyy")
val date = dateFormat.parse("05/25/2020")
println(date)
```

פלט:

```
Mon May 25 00:00:00 EST 2020
```

חקירה עמוקה:

המיתון של תאריך ממחרוזת הוא טכניקה נוחה וחשובה עבור מתכנתים כדי להתאים את התאריך לפורמטים שונים ולבצע פעולות מתאימות עליו. בעבר, תהליך זה נעשה ידנית על ידי היעזרות בספריות נוספות כמו אסמבל. במקרים אחרים, דנת של תאריך ומחרוזת נעשתה באופן אוטומטי על ידי מנועי התצוגה של המחשבים. עם טכנולוגיות כמו Kotlin ופוניטמנט לפי רפרנס, תהליך זה הפך יותר פשוט ונוח עבור מתכנתים.

ראה גם:

למידע נוסף על מיתון תאריך ממחרוזת ב-Kotlin, ניתן להתייחס למדריכים ובלוגים אופניים, כגון המדריך הרשמי של Kotlin ומדריכי התאריכים של קורס קוטלין על פלאטפורמות לומוולס כמו שימושי. למתחילים, מומלץ להתחיל עם המאמרים הבסיסיים על פונקציות בנאי תאריך ב-Kotlin.