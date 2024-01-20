---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט היא תהליך שבו תוכנית מעבירה נתונים מקובץ טקסט למשתנים בתכנית. מתכנתים מבצעים את זה לדעת או לשנות מידע שמאוחסן בקובץ.

## כיצד:

באמצעות קוד C# ,אפשר לקרוא קובץ טקסט. דוגמה:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string text = File.ReadAllText(@"C:\YourTextFile.txt");
        Console.WriteLine(text);
    }
}
```
הפלט יוצג את המידע שקיים בקובץ הטקסט שבחרת.

## צלילה עמוקה

קריאת קבצי טקסט היא חלק בלתי נפרד מהתכנות, ומשמשת מאז בתחילת ימי המחשבים האישיים. למעשה, זהו אחד השימושים הראשונים של החומרה והתוכנה - לשמור מידע בצורה שניתן לקרוא הן למחשב והן לאדם.

כמו כן, ישנן גם אלטרנטיבות לקריאת קבצים. לדוגמה, אתה יכול להשתמש בספריות כדי לקרוא מקובצי XML או CSV, או להשתמש בממשקי API כדי לגשת למידע ממקורות אחרים.

הפרטים של ההגשום של קריאת קובץ טקסט יכולים להשתנות בהתאם להגדרות ההפעלה ולמערכת הפעלה שעליה התוכנית רצה, אך במרבית המקרים, המידע יועבר לזיכרון לפני עיבוד על ידי התוכנית.

## ראה גם

1. [קריאת קובץ טקסט בעזרת StreamReader](https://docs.microsoft.com/he-il/dotnet/api/system.io.streamreader?view=net-5.0)
2. [קריאת קובץ טקסט באופן אסינכרוני](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/concepts/async/)
3. [חיבור לממשקי API לגישה למידע](https://docs.microsoft.com/he-il/learn/modules/csharp-make-http-requests/)