---
title:                "C#: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# למה:

הכתיבה של קובץ טקסט היא חלק חשוב ביישום תוכניות בשפת סי שארפ. קבצי טקסט מאפשרים לנו לשמור ולקרוא מידע, וזהו מקום מבחר נהדר לאחסון נתונים, קוד פייתון, ועוד.

# איך ל:

הנה דוגמא פשוטה של כתיבת קובץ טקסט בשפת סי שארפ:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string text = "זהו טקסט שיכנס לקובץ!";
        File.WriteAllText("myfile.txt", text);
    }
}
```
כשנריץ את הקוד הזה, הוא יכתוב את הטקסט לקובץ בשם "myfile.txt". תוכלו גם לקרוא את הטקסט מקובץ באמצעות פקודה זו:

```C#
string content = File.ReadAllText("myfile.txt");
Console.WriteLine(content);
```
תוכלו להשתמש גם בפקודות נוספות כמו "File.AppendAllText" ו"File.ReadAllLines" כדי להוסיף תוכן לקובץ או לקרוא את התוכן שלו בצורה שונה.

# חפירה עמוקה:

כאשר אנו כותבים קבצי טקסט, יש לנו יכולת להתאים את התוכן והתבניות לצרכים שלנו. ניתן להשתמש בתוספות נוספות כמו "System.Text.Encoding" כדי להגדיר את הקידוד של הקובץ, ובכך לאפשר כתיבה וקריאה של טקסט בשפות ותווים שונים. תוכלו גם להשתמש בפקודות לעיצוב הטקסט, כגון "Console.WriteLine" ו"Console.ReadLine", כדי לשפר את הפלט של הקוד שלכם.

# ראו גם:

- עידכוני טקסט בשפת סי-שארפ: https://docs.microsoft.com/he-il/dotnet/csharp/language-reference/tokens/verbatim
- מדריך לכתיבת קבצי טקסט ב-C#: https://www.tutorialspoint.com/csharp/csharp_files.htm
- שמירה וקריאה של קובץ טקסט עם Stream: https://docs.microsoft.com/he-il/dotnet/standard/io/how-to-write-text-to-a-file