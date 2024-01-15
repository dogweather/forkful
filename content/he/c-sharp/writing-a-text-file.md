---
title:                "כתיבת קובץ טקסט"
html_title:           "C#: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כאשר מתכנתים ב-C# יכולים להשתמש בקבצי טקסט כדי לאחסן ולשנות נתונים בצורה מהירה ויעילה. ניתן להשתמש בקבצי טקסט כדי לשמור מידע מתחילת התהליך התכנותי ועד לשלבים מתקדמים יותר בפיתוח התוכנה.

## איך לעשות זאת?

בשפת C# ישנם מספר דרכים לכתוב קבצי טקסט. הנה כמה דוגמאות של איך ניתן ליצור קובץ טקסט ולשנות אותו בעזרת שפת C#:

```C#
// יצירת קובץ טקסט חדש
string filePath = @"C:\myFile.txt";
using (StreamWriter writer = new StreamWriter(filePath))
{
    writer.WriteLine("זוהי כתבה ראשונה שנכתבת בשפת C#.");
}

// קריאת קובץ טקסט קיים
string filePath = @"C:\myFile.txt";
string fileContent = File.ReadAllText(filePath);
Console.WriteLine(fileContent);

// הוספת תוכן לקובץ טקסט קיים
string filePath = @"C:\myFile.txt";
using (StreamWriter writer = new StreamWriter(filePath, true))
{
    writer.WriteLine("התוספת היא שורה נוספת בסוף הקובץ.");
}
```

בתוך קובץ הקוד, אנו משתמשים במחלקות כמו "StreamWriter" ו-"File" כדי לגשת לקבצי הטקסט ולשנות אותם בדרכים שונות. אחד הדברים החשובים לזכור הוא לסגור את הגישה לקובץ באמצעות הפקודה "using" כדי למנוע קוד כפול.

## נכנסים עמוק יותר

כאשר משתמשים בפקודות כמו "StreamWriter" ו-"File", יש להתעלם מפרטים כמו התנאים המסובכים שמאפשרים קריאה וכתיבה לקבצים באופן פרטני יותר. כמו כן, ניתן לשנות את הכתובת של קובץ הטקסט או להרחיב את הגישה לתיקייה שלמה על ידי שימוש בפקודות נוספות כמו "Directory" ו-"Path".

## ראה