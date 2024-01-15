---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא תהליך נפוץ בתכנות כי הוא מסייע לנו ליצור קבצים שחייבים לקיים לפרק זמן קצר, למשל קבצים מידע או קבצי קובץ שנועדו למחיקה בסיום תהליך מסוים. 

## איך לעשות זאת

ניתן ליצור קובץ זמני בעזרת הפעולה `Path.GetTempFileName()` בשפת C#. להלן דוגמה ליצירת קובץ זמני והדפסת הנתונים השמורים בו:

```C#
string tempFile = Path.GetTempFileName();
File.WriteAllText(tempFile, "Hello World!");
Console.WriteLine(File.ReadAllText(tempFile));
```

פלט:

```
Hello World!
```

## מבולגן עמוק

כאשר משתמשים בפונקציה `Path.GetTempFileName()` נוצר קובץ מקומי עם שם מקונן והרחבת `.tmp` בתוך תיקיית הקבצים הזמניים של המחשב. תיקייה זו תימחק אוטומטית בסיום תהליך היישום. כמו כן, ניתן להשתמש בפונקציה `GetTempPath()` לקבלת נתיב לתיקיית הקבצים הזמניים ולאחר מכן להשתמש בפעולת `Combine()` כדי ליצור קובץ בנתיב הזה. עוד הסברים על יצירת קבצים זמניים ניתן למצוא במקורות המצורפים למטה.

## ראה גם

- [מדריך ליצירת קבצים זמניים ב- C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [משתנים זמניים בהערה על ביצועים בשפת C#](https://www.c-sharpcorner.com/article/the-difference-between-temporary-and-temp-folder-in-c-sharp/)