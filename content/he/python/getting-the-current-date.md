---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
שיבוא של התאריך הנוכחי בפייתון הוא בכדי לקבל מידע או לפעול על פי התאריך הנוכחי או השעה הנוכחית. מתכנתים משתמשים בזאת לשלל צרכים, כולל בדיקת יום השבוע, הדפסת תאריכים יומית, תזמונים, ועוד.

## איך לעשות:
אפשר לקבל את התאריך הנוכחי בפייתון באמצעות הספרייה `datetime`:

```Python
from datetime import date

today = date.today()
print("Today's date:", today)
```
אם אתה מריץ את הקוד שמעלה, אתה צפוי לראות את התאריך הנוכחי כתוצאה.

## שיעור עמוק:
- **הקשר ההיסטורי:** מאז הגרסה 2.3, פייתון כללה את הספרייה `datetime`, שהפכה לחלק לא נפרד מהשפה שלנו ומאפשרת למתכנתים לעבוד בצורה יעילה עם תאריכים וזמנים.
- **חלופות:** ישנן ספריות נוספות כמו `time` ו `calendar` שהן גם מאפשרות ליהנות משלל פעולות זמן ותאריך. אך, `datetime` הפכה לבחירה הפופולרית ביותר.
- **פרטי היישום:** `datetime` מתכנתת בצורה אובייקטיבית. לדוגמה, `date.today()` מחזירה מופע של המחלקה `date` שמייצג את התאריך של היום.

## ראה גם:
קישורים למקורות אחרים:
- הספרייה `datetime` בדוקומנטציה הרשמית של פייתון: https://docs.python.org/3/library/datetime.html
- כיצד להשתמש בספרייה `time` לקבלת הזמן הנוכחי: https://docs.python.org/3/library/time.html
- טיפים וטריקים לעבודה עם תאריכים וזמנים בפייתון: https://realpython.com/python-datetime/