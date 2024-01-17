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

איך ליצור קובץ זמני בשפת C#

## מה & למה?

ישנם מצבים בתכנות כאשר יש צורך ליצור קובץ זמני שנמחק בסופו של דבר. קובץ זמני הוא קובץ שנוצר ומתווסף למערכת הקבצים של המחשב באופן זמני ונמחק כאשר התוכנית מפסיקה לרוץ. בדרך כלל, תוכנית תשתמש בקובץ זמני כדי לשמור נתוני מתמש לזמן מוגבל או כדי לבצע פעולות זמניות בזמן ריצתה.

## איך לעשות זאת:

כדי ליצור קובץ זמני בשפת C#, ניתן להשתמש בפונקציה `GetTempFileName()` מהמחלקה `Path`. למשל:

```C#
string tempFileName = Path.GetTempFileName();
Console.WriteLine(tempFileName);
```

שורת הקוד הזו תדפיס את הנתיב של הקובץ הזמני שנוצר למסך. הנתיב יהיה משתנה על פי מערכת ההפעלה ועל פי הקישוריות של השרת.

מעבר להשתמש בפונקציה `GetTempFileName()`, ניתן להשתמש גם במחלקה `TempFile`, שתסייע ליצור ולנהל קבצים זמניים. לדוגמה:

```C#
using (TempFile tempFile = new TempFile())
{
    string fileName = tempFile.Path;
    Console.WriteLine(fileName);
}
```

שורה זו תדפיס את הנתיב של הקובץ הזמני שנוצר. הנתיב ישתנה לפי מערכת ההפעלה וגם לפי הקישוריות של השרת. כאשר הבלוק `using` מפסיק לרוץ, הקובץ הזמני ימחק אוטומטית.

## חפירה מעמוק:

ישנם מספר דרכים ליצור קובץ זמני בשפת C# המתבססת על מחלקות אחרות כמו `FileStream` ו-`File`. אם יש צורך בקובץ זמני ריק, ניתן להשתמש במחלקה `FileStream` כדי ליצור קובץ ולאחר מכן למחוק אותו. עם זאת, הפתרון הזה אינו מתאים לכל המקרים ובמצבים מסוימים עדיף להשתמש בפונקציה `GetTempFileName()`.

בנוסף, כאשר יש צורך ליצור קובץ זמני מתוך קבצים אחרים, ניתן להשתמש בפונקציות כמו `Copy()` ו- `Replace()` כדי להעתיק תוכן מקובץ קיים לקובץ זמני.

## ראה גם:

למידע נוסף על יצירת קבצים זמניים בשפת C#, ניתן לעיין במסמכים המובנים הבאים:

- [פונקציית GetTempFileName() במאמר הרשמי של Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [מחלקה TempFile במאמר הרשמי של Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.io.tempfile?view=net-5.0)
- [יצירת קבצים זמניים עם ה-FileTempStream Class ב-Code Project](https://www.codeproject.com/Articles/5042/TemporaryFileStream-Class)
- [פתרונות אחרים ליצירת קבצים זמניים ב-C# ב-Stack Overflow](https://stackoverflow.com/questions/2146292/creating-temporary-files-in-c-sharp/2146438)