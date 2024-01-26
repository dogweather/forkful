---
title:                "שימוש במנפה שגיאות"
date:                  2024-01-26T03:48:46.820187-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במנפה שגיאות"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש במנפה זה אומר להתחבר לכלים מיוחדים כדי לבדוק ולאבחן קוד. מתכנתים עושים זאת כדי להרוס באגים, להבין את זרימת הקוד, ולהבטיח שהקוד שלהם מתנהג כפי שהם מצפים - זה כמו להזריק מיקרוסקופ למוח של הקוד שלך.

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
- [תכונות המנפה של JetBrains Rider](https://www.jetbrains.com/rider/features/debugger.html)