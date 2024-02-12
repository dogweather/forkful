---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/c-sharp/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:39.605708-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
