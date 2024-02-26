---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:39.605708-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-C# \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D0\u05D9\u05DE\u05D5\u05EA \u05E9\u05DC \u05E0\u05D5\u05DB\u05D7\u05D5\
  \u05EA \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\u05D1 \u05DE\
  \u05E1\u05D5\u05D9\u05DD \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D9\u05DE\u05E0\
  \u05E2 \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D4\u05E0\
  \u05D9\u05E1\u05D9\u05D5\u05DF \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05D0\u05D5\
  \ \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D0\u05DC\u2026"
lastmod: '2024-02-25T18:49:37.606526-07:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-C# \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05E9\u05DC \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA\
  \ \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\u05D1 \u05DE\u05E1\
  \u05D5\u05D9\u05DD \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\
  \u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D9\u05DE\u05E0\u05E2\
  \ \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D4\u05E0\u05D9\
  \u05E1\u05D9\u05D5\u05DF \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05D0\u05D5 \u05DC\
  \u05DB\u05EA\u05D5\u05D1 \u05D0\u05DC\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה האם ספרייה קיימת ב-C# כוללת אימות של נוכחות תיקייה בנתיב מסוים במערכת הקבצים. מתכנתים עושים זאת כדי להימנע משגיאות כמו הניסיון לקרוא מאו לכתוב אל ספרייה שאינה קיימת, ובכך להבטיח ניהול קבצים ותיקיות חלק יותר.

## איך לעשות:

### באמצעות System.IO

C# מספקת את ה-namespace של `System.IO` שמכיל את המחלקה `Directory`, המציעה דרך ישירה לבדוק אם ספרייה קיימת באמצעות המתודה `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // בדוק אם הספרייה קיימת
        bool directoryExists = Directory.Exists(directoryPath);

        // הדפס את התוצאה
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**דוגמה לפלט:**

```
Directory exists: False
```

במקרה שהספרייה כן קיימת בנתיב `C:\ExampleDirectory`, הפלט יהיה `True`.

### באמצעות System.IO.Abstractions לבדיקות יחידה

כאשר מדובר בהפיכת הקוד שלך לכזה שניתן לבדוקות יחידה, במיוחד כאשר הוא מתקשר עם מערכת הקבצים, החבילה `System.IO.Abstractions` היא בחירה פופולרית. היא מאפשרת לך להפשיט ולהתחזות לפעולות מערכת קבצים בבדיקות שלך. הנה איך אפשר לבדוק אם ספרייה קיימת באמצעות התקרבות זו:

ראשית, וודא שהתקנת את החבילה:

```
Install-Package System.IO.Abstractions
```

לאחר מכן, אתה יכול להזריק `IFileSystem` לתוך המחלקה שלך ולהשתמש בו כדי לבדוק אם ספרייה קיימת, מה שמאפשר בדיקות יחידה קלות יותר.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**דוגמה לפלט:**

```
Directory exists: False
```

התקרבות זו מפרידה בין לוגיקת היישום שלך לבין הגישה הישירה למערכת הקבצים, ובכך הופכת את הקוד שלך ליותר מודולרי, ניתן לבדיקות יחידה ולתחזוקה.
