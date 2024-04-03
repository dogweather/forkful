---
date: 2024-01-26 00:51:37.334842-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1\u05D5\u05D0\u05D5 \u05E0\u05EA\u05D7\
  \u05D9\u05DC \u05E2\u05DD \u05D1\u05DC\u05D5\u05E7 try-catch. \u05D6\u05D4 \u05DB\
  \u05DE\u05D5 \u05DC\u05E9\u05D9\u05DD \u05E8\u05E9\u05EA \u05D1\u05D9\u05D8\u05D7\
  \u05D5\u05DF \u05DE\u05EA\u05D7\u05EA \u05DC\u05D0\u05E7\u05E8\u05D5\u05D1\u05D8\
  \ \u05E2\u05DC \u05D7\u05D1\u05DC \u05D3\u05E7. \u05D0\u05DD \u05D4\u05D5\u05D0\
  \ \u05E0\u05D7\u05DC\u05D9\u05E7, \u05D4\u05D5\u05D0 \u05DC\u05D0 \u05D9\u05E4\u05D5\
  \u05DC \u05DC\u05DE\u05D8\u05D4 - \u05D4\u05D5\u05D0 \u05D9\u05EA\u05E4\u05E1."
lastmod: '2024-03-13T22:44:39.355149-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05EA\u05D7\u05D9\u05DC \u05E2\u05DD \u05D1\
  \u05DC\u05D5\u05E7 try-catch."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

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
