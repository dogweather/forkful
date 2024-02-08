---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
aliases:
- he/c-sharp/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:13:06.753925-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

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
