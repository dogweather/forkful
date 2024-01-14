---
title:                "C#: לבדיקה האם תיקייה קיימת."
simple_title:         "לבדיקה האם תיקייה קיימת."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

לבדיקת האם תיקייה קיימת יש מספר מטרות. בכמה מקרים, ייתכן שנרצה לוודא שהתיקייה קיימת לפני שנמשיך לבצע פעולות אחרות עליה, כולל יצירת קבצים או גישה לתוכן שלה. ניתן גם להשתמש בבדיקת קיום תיקייה בכדי להתמודד עם טיפול בשגיאות נוספות או להראות הודעות מתאימות למשתמש במחשב.

## איך לבדוק אם תיקייה קיימת

```C#
String path = "C:/Users/User/Documents";
if (Directory.Exists(path)) 
{
    Console.WriteLine("התיקייה קיימת.");
} 
else 
{
    Console.WriteLine("התיקייה לא קיימת.");
}
```

### פלט נתונים

```
התיקייה קיימת.
```

## עיון עמוק

בשפת C#, ניתן לבדוק אם תיקייה קיימת באמצעות מתודה פנימית בשם `Directory.Exists()`. המתודה מחזירה ערך בוליאני, כאשר `true` משמעו שתיקייה קיימת ו `false` משמעו שאינה קיימת.

בנוסף, ניתן להשתמש במתודות נוספות כגון `DirectoryInfo.Exists()` ו- `File.Exists()` בכדי לבדוק אם קיים קובץ או תיקייה במקום מסוים.

## ראו גם

- [מדריך על בדיקת קיום תיקייה בשפת C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netframework-4.8)
- [פלטפורמת ספרייוור מפתחים לעסקים בעברית](https://sprayware.co.il/developing/)