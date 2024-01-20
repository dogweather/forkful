---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# מדפיס את ה-Debug Output ב-C# 

## מה ולמה?

מדפיס Debug Output הוא שיטה להציג מידע נוסף במהלך תהליך הקוד. הוא מאפשר לך לבדוק את השגיאות בקוד שלך.

## איך לעשות:

ב-C#, ניתן להדפיס Debug Output באמצעות ממשק `System.Diagnostics.Debug.Write`:

```C#
using System.Diagnostics;

class Program
{
    static void Main()
    {
        Debug.Write("This is a debug message");
    }
}
```

בשביל לראות את המידע, אתה תצטרך לפתוח את "Output" window ב-Visual Studio. כאן תוכל לראות את ההודעות שהודפסו בזמן ריצת התוכנה.

## שיעור מעמיק

שיטת Debug Output נמצאת בשימוש מאז מחצית ה-90 מהמאה ה-20, ומאז נמצאה באמצעות מגוון שפות תכנות.

ישנן שיטות אלטרנטיביות ל-Debug Output, כולל השלמת קוד באמצעות `System.Diagnostics.Trace`. אבל, Debug Output משמש בעיקר במהלך תהליך הפיתוח, בעוד ש-Trace משמש לאחר שהפרויקט הושלם.

פרטי הישום של Debug Output משתנים מאוד, בהתאם לאפליקציה, המערכת המבצעת ושפת התכנות שבה משתמשים. 

## ראה גם 

[דוקומנטציה של Debug Class ב-Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0)
[דוקומנטציה של Trace Class ב-Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace?view=net-5.0)