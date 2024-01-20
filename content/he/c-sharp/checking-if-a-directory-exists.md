---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "C#: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

אימות קיומו של תיקייה הוא תהליך שבו התוכנית מבדיקה אם תיקייה מוגדרת כבר קיימת במערכת הקבצים. מתכנתים עושים את זה למנוע שגיאות כאשר ישנן פעולות הכוללות קריאה או כתיבה לתיקייה.

## איך 

דוגמאות מהקוד ופלט מדגם בתוך בלוקי קוד ```C# ... ```

```C#
using System.IO;

string path = @"C:\folder";

if (!Directory.Exists(path))
{
    Directory.CreateDirectory(path);
}
```

אם התיקייה אינה קיימת, יוֹצֵר את התיקייה שנקבעה בנתיב (path).

## הצצה למטה

אימות קיומו של תיקייה הוא שיטה שהשתמשה מאז שהמערכת הראשונה של ניהול הקבצים קיימה בצורה ממוחשבת. היא אמנם פשוטה ומדויקת, אך ישנם חלופות שמתחילות להופיע, כמו למשל File System Access API של גוגל, שנותן גישה למערכת הקבצים ישירות מהדפדפן.

## ראו גם:

1. [Directory.Exists Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)

2. [Directory.CreateDirectory Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.createdirectory?view=net-5.0)

3. [File System Access API](https://web.dev/file-system-access/)