---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:56:17.062739-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת ב-C# היא תהליך של אימות הקיום וגישה לתיקייה במערכת הקבצים. תוכניתנים עושים זאת כדי למנוע שגיאות בעת ניסיון לקרוא מתיקייה שלא קיימת או לכתוב לתוכה.

## איך לעשות:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string pathToCheck = @"C:\example\path";

        if (Directory.Exists(pathToCheck))
        {
            Console.WriteLine("The directory exists.");
        }
        else
        {
            Console.WriteLine("The directory does not exist.");
        }
    }
}
```

פלט לדוגמא:
```
The directory does not exist.
```
או
```
The directory exists.
```

## עיון נוסף
בהקשר ההיסטורי, בדיקת קיום תיקייה היא חלק מתכנות מחשבים מאז ומעולם. השפה המודרנית של סי-שארפ מעניקה כלים פשוטים לביצוע הפעולה. לחלופין, אפשר להשתמש בכלים של מערכת ההפעלה, אבל `Directory.Exists` הוא הכי ישר ובטוח. פרטי היישום כוללים קריאה ל-API של חלונות אשר מברר את סטטוס התיקייה.

## ראה גם
- [מחלקת Directory במיקרוסופט](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=netcore-3.1)
