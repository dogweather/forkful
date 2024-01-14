---
title:    "C#: מרתיחת תאריך למחרוזת"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

ממשקי משתמש ותוכניות דרישה לפעמים מצריכים המרה של תאריך למחרוזת. כתיבת פונקציה שתעשה זאת בכל פעם עשוי להיות מסורבלת ומבלבלת. איך ניתן להציג בצורה נוחה ומסודרת את התאריך בפורמט שניתן לקריאה על ידי מכונית ואנשים?

## איך לעשות זאת

כדי להמיר תאריך למחרוזת בשפת סי-שארפ, ניתן להשתמש בפונקציית ToString על אובייקט מסוג DateTime. היא מאפשרת להציג את התאריך בפורמט מותאם לצרכי המשתמש כגון "dd/MM/yyyy" או "MMM d, yyyy". להלן דוגמאות קוד ופלט:

```C#
DateTime today = DateTime.Today;
string dateString = today.ToString("dd/MM/yyyy");
Console.WriteLine(dateString); // תצוגת התאריך בפורמט dd/MM/yyyy
string shortDateString = today.ToString("MMM d, yyyy");
Console.WriteLine(shortDateString); // תצוגת התאריך בפורמט MMM d, yyyy
```

## שכיבה עמוקה

פונקציית ToString מאפשרת הרבה יותר ממתן פורמט לתאריך. ניתן להשתמש במגוון מתן תבניות שמאפשרות להציג את התאריך בכל סוג של פורמט שנרצה. הנה כמה דוגמאות נוספות:

```C#
DateTime now = DateTime.Now;
string fullDateFormat = now.ToString("dddd, dd MMMM yyyy"); // "D, d MMMM yyyy"
string timeFormat = now.ToString("hh:mm tt"); // "10:45 PM"
string customFormat = now.ToString("MM-dd-yy HH:mm:ss"); // "07-15-20 22:45:00"
```
 למידע נוסף על הפונקצייה ניתן לעיין במדריך המפורט של Microsoft ל [DateTime.ToString()](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=netcore-3.1).

## ראה גם

- [מדריך לפורמט של תאריך וזמן בסי-שארפ](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [הבדלים בין DateTime.ToString() ל DateTime.ToShortDateString() ל DateTime.ToLongDateString()](https://stackify.com/csharp-datetime-toshortdatestring-tolongdatestring/)
- [תבניות לפונקציית ToString()](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)