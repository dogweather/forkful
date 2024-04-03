---
date: 2024-01-26 01:17:55.208181-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\
  \u05D9\u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\
  \u05EA \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E0\
  \u05E7\u05D5\u05EA \u05E7\u05D5\u05D3, \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\
  \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA, \u05DC\u05D4\u05E4\u05D7\u05D9\u05EA \u05D0\
  \u05EA\u2026"
lastmod: '2024-03-13T22:44:39.356739-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\
  \u05D9\u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\
  \u05EA \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## מה ולמה?

ריפקטורינג הוא התהליך של שינוי מבנה קוד מחשב קיים מבלי לשנות את התנהגותו החיצונית. מתכנתים עושים זאת על מנת לנקות קוד, לשפר את הקריאות, להפחית את המורכבות, ולשפר את התחזוקה.

## איך לעשות:

בואו נרפקטר מתודה פשוטה ב-C# שמחשבת ומדפיסה את סכום של מערך של מספרים:

לפני רפקטורינג:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

אחרי רפקטורינג:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// שימוש:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

על ידי ריפקטורינג, הפרדנו בין דאגות, הפכנו את המחלקה `Calculator` ליותר גמישה על ידי אפשור שימוש בכל מערך של מספרים, ונצלנו את LINQ כדי להפוך את חישוב הסכום ליותר קונציזי.

## צלילה עמוקה

ריפקטורינג שורשיו בקהילת התכנות של Smalltalk והתפרסם בשנות ה-90 על ידי הספר של מרטין פאולר "Refactoring: Improving the Design of Existing Code". במרוצת השנים, הוא הפך לחלק מהותי ממתודולוגיות אג'ייל וממנהגי קידוד טובים.

ישנן גישות שונות לריפקטורינג, כמו Red-Green-Refactor בפיתוח מונחה בדיקות (TDD). זה מבטיח שהריפקטורינג לא מכניס באגים על ידי התחלה עם בדיקה שנכשלת, הפיכתה למעבר, ואז ניקוי הקוד.

כאשר מיישמים ריפקטורינג, קריטי להחזיק בסוויטת בדיקות מקיפה כדי להבטיח שלא נשברת פונקציונליות במהלך התהליך. כלים אוטומטיים לריפקטורינג, כמו ReSharper ל-C#, יכולים גם לעזור בתהליך זה על ידי הצעת דרכים בטוחות לשנות מבני קוד. עם זאת, הכלים אמורים להיות בתוספת להבנה עמוקה של קוד המקור ועקרונות קידוד.

## ראה גם

- המחקר החשוב של מרטין פאולר על ריפקטורינג: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- המדריך של מיקרוסופט לריפקטורינג ב-Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- צפייה מעמיקה בתבניות ריפקטורינג עם דוגמאות: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
