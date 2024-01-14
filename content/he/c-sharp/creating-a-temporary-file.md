---
title:                "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה
בכדי לכתוב קוד פונקציונאלי ויעיל, ייתכן שתצטרך ליצור קבצים זמניים כחלק מתהליך התפתחות התוכנה שלך. ליצור קובץ זמני יכול להיות שימושי במספר דרכים, כגון להעביר מידע בין פונקציות, לאתחל משתנים או לנתח נתונים בצורה נוחה.

## איך ליצור קובץ זמני בשפת C#
ליצור קובץ זמני בשפת C# ניתן בעזרת הפונקציה Path.GetTempFileName() המחזירה את השם המלא של הקובץ הזמני הנוצר. ניתן להשתמש בפונקציה זו באופן הבא:

```C#
string tempFilePath = Path.GetTempFileName();
Console.WriteLine($"Temporary file created: {tempFilePath}");
```

פלט התוכנית יראה כך:

```
Temporary file created: C:\Users\username\AppData\Local\Temp\amisov34.tmp
```

ניתן גם ליצור קובץ זמני באמצעות הפונקציה File.CreateTemp() המחזירה אובייקט מטיפוס FileStream שמאפשר גישה לקובץ הזמני. לדוגמה:

```C#
FileStream tempFile = File.CreateTemp();
string tempFilePath = tempFile.Name;
Console.WriteLine($"Temporary file created: {tempFilePath}");
```

פלט התוכנית יראה כך:

```
Temporary file created: C:\Users\username\AppData\Local\Temp\t3mjpoj4.tmp
```

על מנת למחוק את הקובץ הזמני לאחר השימוש, ניתן להשתמש בפונקציה File.Delete() עם הנתיב של הקובץ הזמני.

## מכניסים עמוק ליצירת קובץ זמני בשפת C#
כאשר אתה משתמש בפונקציות Path.GetTempFileName() או File.CreateTemp() כדי ליצור קובץ זמני, פונקציות אלו מייצרות קובץ ריק במיקום הזמני של המחשב. אלו אומרים, המקום הזמני עלול להיות יישום של הרצת התוכנית. לכן, קובץ זמני יכול להיות שימושי רק בהרצת התוכנית הזו.

כדי ליצור קובץ זמני שיכול