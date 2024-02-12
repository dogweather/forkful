---
title:                "טיפול בשגיאות"
aliases: - /he/c-sharp/handling-errors.md
date:                  2024-01-26T00:51:37.334842-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?

ניהול שגיאות בשפת C# עוסק בהתמודדות עם הלא צפוי - כמו להיתקל בשרוכים. תכניות עלולות להיתקל בנתונים לא נכונים או חיבורים לקויים. אנו טופלים בשגיאות כדי למנוע מהתוכנה שלנו להתרסק, ולאפשר לה להתאושש בחן.

## איך ל:

בואו נתחיל עם בלוק try-catch. זה כמו לשים רשת ביטחון מתחת לאקרובט על חבל דק. אם הוא נחליק, הוא לא יפול למטה - הוא יתפס.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // אופס, האינדקס חורג מהגבולות!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Caught an error: " + e.Message);
        }
    }
}
```

דוגמת פלט כאשר הדברים לא הולכים כצפוי:
```
Caught an error: Index was outside the bounds of the array.
```

עכשיו נוסיף בלוק finally - זה מה שקורה לא משנה מה, כמו לשלם מסים.

```C#
try {
    // קוד עלול לגרום לבעיות כאן
} catch (SomeSpecificException e) {
    // טיפול בשגיאה ספציפית כאן
} finally {
    // קוד זה ירוץ ללא תלות במה שקורה למעלה
    Console.WriteLine("This always runs.");
}
```

## צלילה לעומק

ניהול שגיאות הוא חלק משפת C# מאז יצירתה. לאורך הזמן, התפתח. בימים הראשונים, מתכנתים הסתמכו על קודי החזרה או דגלים גלובליים לאיתות על בעיות - מסורבל ועלול לשגיאות.

C# משתמשת בחריגות, גישה יותר מודרנית. חריגה נזרקת כשקורה הלא צפוי, בדיוק כמו זריקת דגל במשחק כדורגל. ניהול חריגות מובנה עם בלוקים של try, catch ו- finally הופך את ניהול רגעים אלו לברור ולנקי יותר מאשר בדיקת שגיאה מהסוג הישן.

אלטרנטיבות? בטח. יש את `UnhandledExceptionEventHandler` לחריגות שזלגות דרך. או בקוד אסינכרוני, ניהול שגיאות מתהפך קצת עם אובייקטים של `Task` שנושאים עימם את המטען שלהם של חריגות.

פרטי היישום - שקולים לקטנות דפוס - קריטיים. חריגות עלולות להיות מושכות במשאבים, ולהאט את הביצועים אם נזרקות בקלות ראש. לכן, אנו משתמשים בהן למקרים חריגים, ולא לשליטה לוגית שוטפת.

## ראה גם

- [תיעוד רשמי על חריגות ב-C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [מתודולוגיות המומלצות בטיפול בחריגות ב-C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
