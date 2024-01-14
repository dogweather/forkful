---
title:                "C#: יצירת קובץ זמני"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות שבגללן אנשים יכולים להיות מעוניינים ליצור קובץ זמני בתוכניתים שלהם ב-C #. הקובץ הזמני משמש כפתרון זמני ומיועד לשמור מידע בזמן שהתוכנית פועלת אך אינה צריכה להשאר לטווח רב. לדוגמה, הוא יכול לשמור נתוני פעילות או תוצאות מחשבון זמני, אשר נמחקים בסוף התוכנית.

## איך ליצור קובץ זמני ב-C #

כדי ליצור קובץ זמני בתכנית C #, ניתן להשתמש במחלקת `Path` ובאמצעי `GetTempFileName ()`. השתמשו בקוד הבא כדי ליצור קובץ זמני חדש ולהדפיס את הנתיב שלו:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine("Temporary file path: " + tempFilePath);
```

פלט היספרות הוא:

```
Temporary file path: C:\Users\UserName\AppData\Local\Temp\tmp123.tmp
```

כעת, אתם יכולים לעבוד עם הקובץ הזמני החדש כמו קובץ רגיל כדי לשמור ולקרוא מידע.

## לילה עמוקה

יצירת קובץ זמני היא תהליך פשוט במבנה של התכנית שלך, אך כדאי לשים לב לכמה נקודות מרכזיות:

- הקובץ הזמני ימחק אוטומטית כשתוכנית הסיימה לרוץ.
- ניתן לשנות את מיקום הקובץ הזמני על ידי שימוש באמצעים נוספים במחלקת `Path` כמו `GetTempPath ()` ו- `GetRandomFileName ()`.
- אם תרצו לשמור את הקובץ הזמני לטווח זמן ממושך, ניתן להשתמש במחלקת `File` ובאמצעי `Copy ()` כדי להעתיק את הקובץ לתיקיית אחסון קבועה.

## ראו גם

- [מידע נוסף על מחלקת `Path` (MSDN)](https://docs.microsoft.com/en-us/dotnet/api/system.io.path)
- [דוגמאות ליצירת קבצים זמני