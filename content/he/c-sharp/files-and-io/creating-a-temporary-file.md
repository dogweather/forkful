---
title:                "יצירת קובץ זמני"
aliases:
- /he/c-sharp/creating-a-temporary-file.md
date:                  2024-01-20T17:40:37.623023-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
יצירת קובץ זמני היא שיטה לפתח קובץ שנמחק בסיום השימוש בו. תכניתנים עושים זאת כדי להבטיח אחסון בטוח ונקי לנתונים זמניים.

## איך לעשות: (How to:)
כדי ליצור קובץ זמני ב-C#, אתה יכול להשתמש במחלקת `Path` ו`File` מה-namespace `System.IO`:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // יצירת נתיב לקובץ זמני
        string tempFilePath = Path.GetTempFileName();

        // כתיבת טקסט לקובץ הזמני
        File.WriteAllText(tempFilePath, "Hello, Temp File!");

        // הדפסת נתונים מהקובץ הזמני
        Console.WriteLine(File.ReadAllText(tempFilePath));

        // מחיקת הקובץ בסיום השימוש
        File.Delete(tempFilePath);
    }
}
```
פלט לדוגמא:
```
Hello, Temp File!
```

## עיון מעמיק: (Deep Dive)
בעבר, קבצים זמניים היו חיוניים לניהול זיכרון במערכות עם משאבים מוגבלים. היום, הם עדיין שימושיים למקרים כמו שמירת נתונים גדולים במהלך עיבוד או כאשר ברצונך להימנע מזיהום מאגרי שיתוף. חלופה היא שימוש במנגנון זיכרון דינמי, כמו זיכרון מתועת (streaming) או מערכת מטמון. ברמה המימושית, ב-C# קיימת טיפול אוטומטי בקבצים זמניים, כולל יצירתם ומחיקתם באמצעות הכלים שסופקו ב-namespace `System.IO`.

## ראה גם: (See Also)
- [מחלקת Path](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [מחלקת File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Namespace System.IO](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=netcore-3.1)
- [ניהול זיכרון ב-C#](https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/)
