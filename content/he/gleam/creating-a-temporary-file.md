---
title:                "יצירת קובץ זמני"
date:                  2024-01-20T17:40:23.365767-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך בו נוצר קובץ לשימוש אחת ולתמיד, או לתקופה קצרה במקום לשימוש מתמשך. תוכניתנים עושים את זה לשם טיפול בנתונים בצורה בטוחה ובלי להעמיס על מערכת הקבצים הראשית.

## איך לעשות:
בגלים, יצירת קובץ זמני עדיין אינה תומכת כחלק מהסטנדרט, אז תצטרך לעבוד עם המערכת שלך כדי ליצור זמניים. זה יכול להיראות כך:

```gleam
external fn mkstemp(template: String) -> Result(Int, Nil) =
  "stdlib.h" "mkstemp"

pub fn create_temp_file() -> Result(Int, Nil) {
  mkstemp("temp.XXXXXX")
}
```

כאשר הפונקציה `mkstemp()` נקראת, היא מחזירה ערך המייצג את מקטע הקובץ, או שגיאה במקרה של כשל. הסטרינג "temp.XXXXXX" מסמל את התבנית שהשם של הקובץ הזמני יורכב ממנה, עם ה-Xים המוחלפים במספרים ואותיות כדי ליצור שם ייחודי.

## עמוק יותר
יום אחד, בעבר, כל הניהול של קבצים זמניים היה על המתכנת. מאז, נולדו פונקציות כמו `mkstemp` שמאבטחות את התהליך וממנעות קונפליקטים ופגיעות בבטיחות. אם Gleam לא יוכל לעזור, שפות כמו C יכולים להיקרא בחיצוניות כדי לבצע זאת. חשוב לזכור שאחריות למחיקת הקובץ הזמני לאחר שסיימת איתו נופלת על המתכנת.

## ראה גם
- המסמך הרשמי של פונקציות ליצירת קבצים זמניים ב-C: [mkstemp](https://man7.org/linux/man-pages/man3/mkstemp.3.html)
- דוקומנטציה של Gleam על פונקציות חיצוניות: [Gleam External Functions](https://gleam.run/book/tour/external-functions.html)
