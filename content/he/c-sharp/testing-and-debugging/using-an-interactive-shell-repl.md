---
date: 2024-01-26 04:13:06.753925-07:00
description: "REPL, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\
  \u05D0\u05D4-\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4, \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05E7\u05DC\u05D9\u05D3 \u05E7\u05D5\
  \u05D3 C# \u05D5\u05DC\u05D4\u05E8\u05D9\u05E5 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D6\u05D4 \u05DC\u05E0\u05D9\u05E1\u05D5\u05D9\u05D9\u05DD \u05DE\u05D4\
  \u05D9\u05E8\u05D9\u05DD, \u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2, \u05D0\
  \u05D5 \u05DC\u05DE\u05D9\u05D3\u05D4 \u05E9\u05DC C#, \u05D1\u05DC\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.345511-06:00'
model: gpt-4-0125-preview
summary: "REPL, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\u05EA \u05E7\u05E8\u05D9\u05D0\
  \u05D4-\u05D4\u05E2\u05E8\u05DB\u05D4-\u05D4\u05D3\u05E4\u05E1\u05D4, \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05D4\u05E7\u05DC\u05D9\u05D3 \u05E7\u05D5\u05D3\
  \ C# \u05D5\u05DC\u05D4\u05E8\u05D9\u05E5 \u05D0\u05D5\u05EA\u05D5 \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D6\u05D4 \u05DC\u05E0\u05D9\u05E1\u05D5\u05D9\u05D9\u05DD \u05DE\u05D4\
  \u05D9\u05E8\u05D9\u05DD, \u05D3\u05D9\u05D1\u05D0\u05D2\u05D9\u05E0\u05D2, \u05D0\
  \u05D5 \u05DC\u05DE\u05D9\u05D3\u05D4 \u05E9\u05DC C#, \u05D1\u05DC\u05D9\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## מה ולמה?
REPL, או לולאת קריאה-הערכה-הדפסה, מאפשרת להקליד קוד C# ולהריץ אותו באופן אינטראקטיבי. תכנתים משתמשים בזה לניסויים מהירים, דיבאגינג, או למידה של C#, בלי הטרחה של הקמת פרויקטים מלאים.

## איך לעשות:
הפעל REPL בסביבת ה-C# שלך באמצעות חלון האינטראקטיבי של C# או הרץ `dotnet-script` בטרמינל שלך. הנה טעימה מהשימוש בזה:

```csharp
> var greeting = "Hello, REPL!";
> Console.WriteLine(greeting);
שלום, REPL!
> 
```

אתה מקבל משוב מיידי. אין צורך לקומפל ולהריץ. רק לכתוב ולראות.

## צלילה לעומק
REPL עשתה את דרכה מ-Lisp לשפות מודרניות, והצליחה במיוחד בשפות דינמיות כמו Python. ב-C#, Roslyn הביאה את ה-REPL קרוב יותר למפתחים. `csi` עבור Roslyn, ו-`dotnet-script` עבור .NET Core, הם אופציות טובות. מבט מעמיק יותר: הם מעריכים קוד לפי שורה, ולא הכל יחד, דגם ההפעלה שונה ביחס לאפליקציות C# טיפוסיות. זה משפיע על שמירת המצב בין הרצות ועל ההיקף של המשתנים.

חלון האינטראקטיבי של C# ב-Visual Studio הוא REPL המופעל על ידי Roslyn. יש לו Intellisense, ייבואים מרובים, ותמיכה בחבילות NuGet. זה צעד גדול מניסויים ראשוניים בשורת הפקודה.

לשפות אחרות, Python משתמשת ב-`IDLE`, ל-JavaScript יש את REPL של Node.js, ו-F# מגיעה עם `F# Interactive`. כל אחת מהן מעודדת לולאות משוב מיידי, יקרות ערך לבדיקת קטעי קוד קטנים או הבנת תכונות של השפה.

## ראה גם
- [REPL של `dotnet-script` ל-.NET Core](https://github.com/filipw/dotnet-script)
