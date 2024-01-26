---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
לכתוב לסטנדרט ארור (standard error) זה להפנות את השגיאות והודעות הדיבאג לפלט מיוחד, נפרד מהפלט הרגיל הנקרא סטנדרט אאוט (standard output). תכנתים זה כדי להפריד בין תוצאות תוכנית לבין הודעות שגיאה ולאפשר ניתוח ומעקב בקלות אחרי בעיות.

## How to:
ככה כותבים ל-stde:
```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("הפלט הרגיל");
        Console.Error.WriteLine("שגיאה: משהו השתבש");
    }
}
```
הפלט:
```
הפלט הרגיל
שגיאה: משהו השתבש
```

## Deep Dive
בהיסטוריה, ההפרדה בין stde ל-stdeo הייתה חשובה במערכות יוניקס כדי להפריד בין לוגים לפלט התוכנית. אלטרנטיבה היא לכתוב לקובץ לוג במקום ל-stderr. ב-C#, Console.Error הוא פשוט StreamWriter שמכוון אוטומטית לפלט השגיאה של המערכת.

## See Also
- מדריך ל-C# Console Class: [https://docs.microsoft.com/en-us/dotnet/api/system.console](https://docs.microsoft.com/en-us/dotnet/api/system.console)
- מדריך לשימוש ב-StreamWriter ב-C#: [https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
