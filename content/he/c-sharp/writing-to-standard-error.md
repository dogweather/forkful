---
title:                "כתיבה לשגיאת התאמה סטנדרטית"
html_title:           "C#: כתיבה לשגיאת התאמה סטנדרטית"
simple_title:         "כתיבה לשגיאת התאמה סטנדרטית"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מדוע:

כדי שהתוכנית שלך תהיה יותר התקדמותית ותספק מידע חשוב לקריאה, יתכן שתרצה להשתמש בכניסה לשגיאות תקינה בקוד שלך.

## איך לכתוב לפלט שגיאה

הנה דוגמאות של כיצד ניתן להשתמש בכניסה לשגיאות בקוד C#:

```C#
try
{
    // קוד שלך כאן
}
catch (Exception ex)
{
    Console.Error.WriteLine(ex.Message);
    // כיוון שאנו משתמשים בכניסה לשגיאות תקינה, אנו יכולים גם להוסיף נתונים נוספים חשובים לפלט שגיאה שאליו אנשים יכולים להתייחס כדי לאתר את הבעיה ולתקנה.
}
```

פלט:

```
אירעה שגיאה בשורה 5: "אין שם תוכנית מוגדר". 
```

## נכנסים עומק יותר:

פקודת `Console.Error` היא פקודה מובנית שמאפשרת לנו להכתיב לפלט שגיאה במקרה ונתקלנו בשגיאות בקוד שלנו. אנחנו יכולים גם להתאים את פלט השגיאה הזה על ידי הוספת נתונים נוספים מתוך השורה שעליה השגיאה אירעה כמו שמוצג בדוגמה הבאה:

```C#
using System;

namespace ErrorExample
{
    class Program
    {
        static void Main(string[] args)
        {
            string name = "";

            try
            {
                Console.WriteLine("הכנס את שמך:");
                name = Console.ReadLine();

                if (string.IsNullOrEmpty(name))
                {
                    throw new Exception("אין שם מוגדר");
                }

                Console.WriteLine("שלום" + name + "!");
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("אירעה שגיאה בשורה " + ex.StackTrace[ex.StackTrace.Length - 1] + ": " + ex.Message);
            }
        }
    }
}
```

פלט:

```
אירעה שגיאה בשורה 10: "אין שם מוגדר".
```

## ראו גם:

- [מדריך לפקודת Console בC#](https://www.c-sharpcorner.com/article/C-Sharp-console-command-help/)
- [המדריך המלא לכתיבת פלט שגיאות איכותי בקוד C#](https://www.c-sharpcorner.com/article/csharp-writing-quality-error-output/)