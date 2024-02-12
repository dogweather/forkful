---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:09:25.812387-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
איחזור התאריך הנוכחי ב-Bash כרוך בשימוש בפקודות מובנות להצגת התאריך והשעה במגוון פורמטים. מתכנתים משתמשים בפונקציונליות זו עבור משימות כמו הוספת חותמת זמן ללוגים, תזמון משימות או סתם כחלק מסקריפטים למידע מערכת על מנת לעקוב אחרי מועדי ביצוע פעולות.

## איך לעשות:
ב-Bash, הפקודה `date` היא הכלי העיקרי שלך לקבלת התאריך והשעה הנוכחיים. הנה כמה דוגמאות לכיצד להשתמש בה:

1. **לקבל את התאריך והשעה הנוכחיים בפורמט הברירת מחדל:**

```bash
date
```

*דוגמה לפלט:*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **התאמה אישית של פורמט הפלט:** אתה יכול לציין את פורמט הפלט באמצעות המפרטים `+%`. לדוגמה, להצגת התאריך בפורמט YYYY-MM-DD:

```bash
date "+%Y-%m-%d"
```

*דוגמה לפלט:*
```
2023-04-05
```

3. **לקבל את הזמן בפורמט חותמת UNIX הנוכחית:** חותמת UNIX היא מספר השניות מאז העידן של UNIX (1 בינואר 1970). זה מועיל לסקריפטים שביצוע חישובים על בסיס הפרשי זמנים.

```bash
date "+%s"
```

*דוגמה לפלט:*
```
1672877344
```

לא נהוג להשתמש בספריות צד שלישי פופולריות עבור פעולה בסיסית זו ב-Bash מאחר והפקודה המובנית `date` מספקת פונקציונליות מקיפה. עם זאת, למען מניפולציות תאריך ושעה מתקדמות יותר, מתכנתים עשויים להשתמש בשפות תכנות אחרות או כלים המציעים ספריות עבור חישובים עם תאריכים וניתוחים שלהם, כמו מודול `datetime` של Python.