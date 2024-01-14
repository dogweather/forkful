---
title:                "C#: לקבל את התאריך הנוכחי"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

כנראה שכבר הרבה פעמים נתקלתם בצורך לקבל את התאריך הנוכחי במחשב שלכם. זה יכול להיות בכדי להציגו למשתמש, ליצור פעילויות על פי מועד שנקרא וכו'. במאמר הזה נלמד באיך מקבלים את התאריך הנוכחי בשפת תכנות C#.

## איך לעשות זאת

מתחילים עם פונקציית המשוב החיונית Now() המוגדרת בספריית System. על ידי קריאה לפונקציה תחזיר היא את התאריך הנוכחי בפורמט המתאים לאזור הזמן של מחשב השרת שלכם.

```c#
using System;
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

רציף נדפיס בקונסול את התאריך הנוכחי בפורמט - MONTH/DAY/YEAR TIME (AM/PM). במקרים שבהם רוצים לקבל רק את התאריך עצמו בלי השעה, נוכל להשתמש בדרך אחרת שנכונה לכתיבת הפורמט הנוכחי.

```c#
using System;
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToShortDateString());
```

הפונקציה ToShortDateString() תחזיר רק את התאריך בפורמט - MONTH/DAY/YEAR. אם ברצונינו לקבל תאריך מתומחר יותר, עלינו להשתמש בפונקציות נוספות.

```c#
using System;
DateTime currentDate = DateTime.Now;
string month = currentDate.ToString("MMMM");
int day = currentDate.Day;
int year = currentDate.Year;
Console.WriteLine(month + " " + day + ", " + year);
```

השורה הראשונה תחזיר את שם החודש באנגלית השנייה תחזיר את היום הנוכחי והאחרונה תחזיר את השנה בפורמט המתאים. מעבר לכך, נוכל גם לקבל את היום הנוכחי בשבוע בעזרת הפונקציה - ToDayOfWeekString().

## לחקור עמוק יותר

כמו כן, ניתן לבצע פעולות נוספות כמו הוספת/הסרה של ימים, חודשים ושנים לתאריך הנוכחי באמצעות הפונקציות