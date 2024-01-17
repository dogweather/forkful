---
title:                "לבדיקת קיום תיקייה"
html_title:           "C#: לבדיקת קיום תיקייה"
simple_title:         "לבדיקת קיום תיקייה"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
אימות אם תיקייה קיימת הוא כלי מפתח שמאפשר למתכנתים לבדוק אם תיקייה קיימת במערכת קבצים של המחשב. מתכנתים משתמשים בכך כדי לוודא שתיקייה מסוימת שהם צריכים לגשת אליה אכן קיימת לפני שהם ממשיכים לפעולות נוספות.

## כיצד לבצע:
כדי לבדוק אם תיקייה קיימת באמצעות C#, ניתן להשתמש בפונקציה בשם `Directory.Exists ()` כדי לבדוק את כתובת התיקייה. אם התיקייה קיימת, הפונקציה תחזיר אמת (true), אחרת היא תחזיר שקר (false). 

```C#
if (Directory.Exists(path))
{
    Console.WriteLine("The directory exists.");
}
else
{
    Console.WriteLine("The directory does not exist.");
}
```

פלט:
```
The directory exists.
```

## חקירה מעמיקה:
### הקשר ההיסטורי:
בעבר, במערכות ההפעלה הישנות, לא היו כלים מובנים לבדיקת תיקיות. מתכנתים נאלצו להשתמש בפקודות של מערכות ההפעלה כדי לבדוק אם תיקייה קיימת. בשנים האחרונות, כשמערכות ההפעלה התפתחו והופיעו כוללים יותר לפיתוח תוכנה, פונקציונליות חדשה נוצרה כדי לבצע את הבדיקה אוטומטית.

### אלטרנטיבות:
כדי לבדוק אם תיקייה קיימת באמצעות C#, גם ניתן לחבר אם משתמש בפונקציה `File.GetAttributes ()` ולבדוק אם כתב האיחסון שמייצג את התיקייה הקיימת. פונקציה זו מחזירה את המאפיינים של כתב האיחסון, אחרת תוכנית שלנו יכולה לשגר שגיאה. 

### פירוט המימוש:
כאשר נכתב קוד עם C#, קפצנו מיד ל- .NET Framework. ב- .NET Framework בנוי `Directory.Exists ()` כדי לבדוק לא רק אם התיקייה קיימת אלא גם כמה מאפיינים נוספים. עם הגדרות תקינות במקום, הפונקציה יכולה לבדוק כיצד הלא מעט קובץ / מאפיינים של תיקייה כי

## ראו גם:
* [יעודים לאמיתיים מאמת הנתיב] (https://docs.microsoft.com/en-us/dotnet/standard/io/file-path-formats#real-path-relevance)
* [מאמת את הנתיב המלא בשרתים ברשת] (https://msdn.microsoft.com/en-us/library/system.IO.path.getfullpath.aspx)