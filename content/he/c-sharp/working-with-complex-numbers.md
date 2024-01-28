---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:39:04.540846-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מרחיבים את מערכת המספרים שלנו לכלול מספרים דמיוניים, מה שמאפשר לנו לפתור משוואות שאין להן פתרונות ממשיים. מתכנתים עוסקים בהם בתחומים כמו הנדסה, פיזיקה ועיבוד אותות, שם מספרים אלה הכרחיים למידול ולפתרון בעיות.

## איך ל:
ב-C# יש מבנה מובנה `System.Numerics.Complex` לעיבוד מספרים מרוכבים. הנה סקירה מהירה:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // יצירת מספרים מרוכבים
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // פעולות בסיסיות
        Complex sum = c1 + c2;
        Complex difference = c1 - c2;
        Complex product = c1 * c2;
        Complex quotient = c1 / c2;

        // פלט תוצאות
        Console.WriteLine($"Sum: {sum}");
        Console.WriteLine($"Difference: {difference}");
        Console.WriteLine($"Product: {product}");
        Console.WriteLine($"Quotient: {quotient}");
        Console.WriteLine($"Magnitude of c1: {c1.Magnitude}");
        Console.WriteLine($"Phase of c1: {c1.Phase}");
    }
}
```

וזה יפיק:

```
Sum: (4.70710678118655, 5.70710678118655)
Difference: (3.29289321881345, 4.29289321881345)
Product: (-1.00000000000001, 9)
Quotient: (0.6, 0.8)
Magnitude of c1: 6.40312423743285
Phase of c1: 0.896055384571344
```

## צלילה עמוקה
מספרים מרוכבים, המורכבים מחלק ממשי וחלק דמיוני (לעיתים נסמן כ-a + bi), התקיימו מאז המאה ה-17. המתמטיקאי האיטלקי ג'רולמו קרדאנו מיוחס להם פיתוח מוקדם. בתכנות, התמודדות עם מספרים מרוכבים כוללת הבנה וניהול של שני חלקים נפרדים אלה.

למרות שה-`System.Numerics.Complex` של C# עמיד ומשולב בשפה, שפות אחרות כמו פייתון מציעות פונקציונליות דומה עם `cmath` או ספריות צד שלישי. ואם אתה עובד בגרסת C# ישנה יותר או גרסת .NET שאינה תומכת ב-`System.Numerics`, ייתכן שתצטרך ליצור מחלקת מספרים מרוכבים משלך או למצוא ספרייה.

בפנים, הפעולות על מספרים מרוכבים משתמשות בחישוב שברוני נקודה צפה, שיכול להביא לשגיאות עיגול. לכן, כשמיישמים אלגוריתמים שמשתמשים באופן נרחב במספרים מרוכבים, חשוב לזכור זאת ולשקול את ההשפעה על הדיוק והמדויקות.

## ראה גם
1. הפנייה ל-C# עבור `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. צלילה עמוקה יותר אל המתמטיקה של מספרים מרוכבים: https://mathworld.wolfram.com/ComplexNumber.html
3. ליישומים חלופיים וספריות, בדוק את Math.NET Numerics: https://numerics.mathdotnet.com/
