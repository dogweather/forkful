---
title:    "C#: לקבלת התאריך הנוכחי"
keywords: ["C#"]
---

{{< edit_this_page >}}

אתנחדד למתקדם נערכתה נדרשת התאריך נוכחי? 

מתקדם, אנחנו לפעמים צריכים להשתמש בתאריך הנוכחי בתכניות המתוכנתות שלנו. הוא יכול לשמש כמוניטור זמן בזמן פעולה, לבדיקת הנתיב האותיות שהוכנסו או להדפסת תאריך לאירוע מסוים.

 איך לעשות זאת

בדרך כלל, מתוך החלון תוכנה, תנועת העכבה מוחתמת את התירגום הוא מביא לזוטות הזמן והתאריך עדכון נכון. אם אתם משתמשים בשפת C#, ישנם כמה דרכים שונות להשיג את התאריך הנוכחי. אחת הדרכים הפשוטות ביותר היא להשתמש בפעולת "DateTime.Now". הנה מסכת קוד קטנה כדי להדגים איך לעשות את זה:

```C#
using System;

namespace CurrentDate
{
  public class Program
  {
    public static void Main()
    {
      DateTime currentDateTime = DateTime.Now;
      Console.WriteLine("The current date and time is: {0}", currentDateTime);
    }
  }
}
```

פלט:

```
See Also

* https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now
* https://www.geeksforgeeks.org/datetime-now-property-method-in-c-sharp/
* https://www.tutorialsteacher.com/csharp/csharp-datetime