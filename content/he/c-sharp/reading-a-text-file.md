---
title:    "C#: קריאת קובץ טקסט"
keywords: ["C#"]
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא כלי חשוב לתכנות בשפת C#. אנשים קוראם כדי לטעון נתונים מתוך קובץ טקסט ולעבד אותם בתוך התוכנית שלהם.

## איך לעשות זאת

```C#
// קוד לקריאת קובץ טקסט
string filePath = @"C:\myFile.txt"; // הגדרת נתיב לקובץ
string[] lines = System.IO.File.ReadAllLines(filePath); // קריאת כל השורות מהקובץ ואיחזורם למערך

// דוגמה לטפסות מיוצאות עם המידע מהקובץ
foreach(string line in lines) // לולאה על כל השורות במערך
{
    string[] data = line.Split(','); // חלוקת השורה לנתונים לפי הפסיקים
    Console.WriteLine($"שם: {data[0]}, גיל: {data[1]}, עיר: {data[2]}"); // הדפסת הנתונים מהשורה
}
```

### דוגמה לפלט:

שם: יונתן, גיל: 25, עיר: ירושלים
שם: רועי, גיל: 30, עיר: תל אביב
שם: ליאת, גיל: 28, עיר: חיפה

## חפירה מעמוק

קריאת קובץ טקסט מאפשרת לנו לעשות המון דברים מועילים כגון עיבוד נתונים, התאמת אינפוזיציה ופריסת נתונים בדרכים שונות. ניתן גם להשתמש בשיטות נוספות כגון `File.Open()` ו`StreamReader` כדי ליצור יישומים מתקדמים יותר.

## ראה גם

- [מדריך לשימוש בקבצי טקסט בשפת C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-line-by-line)
- [תסמונות לשימוש בקבצי טקסט בשפת C#](https://www.tutorialspoint.com/csharp/csharp_reading_from_text_files.htm)
- [מדריך של MSDN על קריאת קבצי טקסט ב-C#](https://msdn.microsoft.com/en-us/library/ezwyzy7b(v=vs.110).aspx)