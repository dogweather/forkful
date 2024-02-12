---
title:                "הגדלת אותיות במחרוזת"
aliases: - /he/c-sharp/capitalizing-a-string.md
date:                  2024-02-03T19:06:04.956682-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
הגדלת אות ראשית של מחרוזת ב-C# כוללת המרה של התו הראשון של מחרוזת לאות רישית אם היא כבר לא כזו. שינוי זה יכול להיות קריטי לעיצוב פלטים, אכיפת סטנדרטים בקידוד, או הפיכת טקסטים בממשק המשתמש לקריאים יותר.

## איך לעשות זאת:
C# מציעה גישה ישירה להגדלת אותיות במחרוזות באמצעות שיטות מובנות. הדרך הפשוטה ביותר לבצע זאת היא על ידי שינוי המחרוזת ישירות באמצעות שיטות אלה. עבור כללי הגדלה מורכבים או ספציפיים יותר (למשל, הגדלת כל מילה), עשויים להיות נחוצים ספריות חיצוניות או שיטות ידניות. להלן דוגמאות המדגימות איך להגדיל אותיות במחרוזת בדרכים שונות ב-C#.

### הגדלת אות ראשונה בסיסית:
להגדלת האות הראשונה של מילה אחת או משפט:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // פלט: "Hello world"
```

### הגדלת כל מילה:
להגדלת האות הראשונה של כל מילה במחרוזת, ניתן להשתמש בשיטת `TextInfo.ToTitleCase` הנמצאת במרחב השמות `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // פלט: "Hello World"
```

שימו לב: `ToTitleCase` לא מורידה את רמת האותיות של שאר האותיות; היא רק משנה לאות רישית את האות הראשונה של כל מילה. כמו כן, מילים מסוימות בכללי כתיבת שם פרטי (כמו "and", "or", "of") עשויות לא להיכלל בתלות בהגדרות התרבות.

### שימוש בשיטות הרחבה לשימוש חוזר:
ניתן ליצור שיטת הרחבה עבור המחלקה `string` כדי לפשט את תהליך ההגדלה, הופך את הקוד שלכם נקי ונוח יותר לשימוש חוזר. הנה איך ליצור ולהשתמש בשיטה כזו:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // פלט: "Hello world"
    }
}
```

שיטת ההרחבה `Capitalize` יכולה להיקרא על כל אובייקט מחרוזת במרחב השמות, מציעה גישה אינטואיטיבית ומונחית-עצמים יותר לניהול מחרוזות ב-C#.

### ספריות צד שלישי:
למרות שספריית הסטנדרט של C# מכסה את רוב הצרכים להגדלת מחרוזות, משימות מתמחות מסוימות עשויות להרוויח מספריות צד שלישי, כמו Humanizer. עם זאת, למשימה של הגדלת מחרוזות בפשטות או כל מילה במחרוזת, שיטות C# הסטנדרטיות הן נאותות ויעילות, מבטלות את הצורך בתלות חיצוניות.
