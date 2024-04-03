---
date: 2024-01-26 03:48:46.820187-07:00
description: "\u05D3\u05DE\u05D9\u05D9\u05E0\u05D5 \u05E9\u05D9\u05E9 \u05DC\u05DB\
  \u05DD \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E7\u05D8\u05E0\u05D4 \u05E9\u05DC\
  \u05D0 \u05DE\u05EA\u05E0\u05D4\u05D2\u05EA \u05DB\u05E8\u05D0\u05D5\u05D9: ```C#\
  \ static void Main() { int result = Sum(1, 2); Console.WriteLine(result); } static\
  \ int Sum(int a, int b)\u2026"
lastmod: '2024-03-13T22:44:39.350403-06:00'
model: gpt-4-0125-preview
summary: "\u05D3\u05DE\u05D9\u05D9\u05E0\u05D5 \u05E9\u05D9\u05E9 \u05DC\u05DB\u05DD\
  \ \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E7\u05D8\u05E0\u05D4 \u05E9\u05DC\u05D0\
  \ \u05DE\u05EA\u05E0\u05D4\u05D2\u05EA \u05DB\u05E8\u05D0\u05D5\u05D9:\n\n```C#\n\
  static void Main()\n{\n    int result = Sum(1, 2);\n    Console."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות:
דמיינו שיש לכם תוכנית קטנה שלא מתנהגת כראוי:

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // אופס, אמור להיות a + b
}
```

באמצעות מנפה של Visual Studio, הגדרו נקודת עצירה על ידי לחיצה על חלק השמאלי ליד `return a + a;`. כשאתם מריצים את התוכנית (עם F5), הביצוע יעצר שם. עברו על משתנים כדי לבדוק את ערכיהם, או השתמשו בחלון המיידית כדי להעריך ביטויים. תראו ש`a` הוא 1 ו`b` הוא 2, אבל `a + a` אינו הסכום שציפינו לו. שנו אותו ל`a + b`, המשיכו להריץ (F5), והנה, הקונסול פולטת 3.

## צלילה לעומק
ההיסטוריה של ניפוי באגים חוזרת אחורה עד לשנות ה-40, כשבאג אמיתי (עש) נמצא במחשב מוקדם. מנפי היום, כמו זה שב-Visual Studio, מספקים סוויטת תכונות עוצמתית, כולל נקודות עצירה, ביצוע צעד אחר צעד, חלונות צפייה, ועוד.

אלטרנטיבות למנפה של Visual Studio כוללות אפשרויות קוד פתוח כמו GDB לשפות מסוג C או pdb ל-Python, וסביבות פיתוח משותפות כמו JetBrains Rider או VS Code שמציעות כלים לניפוי באגים ל-C# ושפות אחרות.

כשאתם צוללים למימוש של מנפה, אתם מסתכלים על תוכנה שמתחברת לתהליך של היישום שלכם. היא מפרשת קוד מכונה, מנהלת מצב זיכרון, ושולטת בזרימת הביצוע. זה חומר כבד שחיוני לניפוי באגים יעיל, וזו הסיבה שמצב ה-debug לרוב רץ לאט יותר ממצב ה-release שבו לא קיימים העגינות הללו.

## ראה גם
- [תיעוד מנפה של Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [אסטרטגיות לניפוי באגים](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
