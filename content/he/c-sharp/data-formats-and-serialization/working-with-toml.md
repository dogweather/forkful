---
date: 2024-01-26 04:20:50.898725-07:00
description: "TOML \u05D4\u05D9\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\
  \u05EA \u05E9\u05DC Tom's Obvious, Minimal Language, \u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05E7\u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05E7\u05DC \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05D1\u05E9\u05DC \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\
  \u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC\u05D5. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D5 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\
  \u05D4, \u05DE\u05E4\u05E9\u05D8\u05D9\u05DD \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.381378-06:00'
model: gpt-4-0125-preview
summary: "TOML \u05D4\u05D9\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\u05D5\
  \u05EA \u05E9\u05DC Tom's Obvious, Minimal Language, \u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05E7\u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05E7\u05DC \u05DC\
  \u05E7\u05E8\u05D5\u05D0 \u05D1\u05E9\u05DC \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\
  \u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC\u05D5."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## כיצד ל:
ראשית, התקן מנתח TOML כמו `Tomlyn`. השתמש במנהל החבילות שלך:

```csharp
dotnet add package Tomlyn
```

לאחר מכן, נתח קובץ TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"בעלים: {tomlTable["owner"]["name"]}");
// פלט:
// בעלים: Tom Preston-Werner
```

עכשיו, צור וכתוב TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML נכתב ל- config.toml");
// פלט:
// TOML נכתב ל- config.toml
```

## לעומק:
TOML נוצר על ידי טום פרסטון-וורנר, השותף המייסד של GitHub, בערך בשנת 2013 כתגובה למגבלות של פורמטים קיימים כמו YAML ו-JSON בהגדרות תצורה. הוא תוכנן במיוחד לקבצי תצורה עם דגש רב על פשטות וחד-משמעיות.

פורמטי תצורה חלופיים כוללים YAML, JSON, ו-XML. עם זאת, TOML בולט בכך שהוא ידידותי יותר לאדם, במיוחד עבור קבצי תצורה שעריכה בהם באופן ידני היא נפוצה. JSON, למרות שהוא נפוץ בכל מקום, פחות קריא עבור תצורות מורכבות, ו-XML מפורט. YAML, למרות שדומה בקריאות, יכול להיות מורכב עם שימוש כבד ברווחים לבנים ויש לו סיכוני אבטחה עם תוכן מסוים.

מבחינת יישום, TOML מתמקד במיפוי נקי לטבלת האש, דבר שהופך את חילוץ הנתונים לניתן לחיזוי. עם שחרור גרסה 1.0.0, TOML חיזק את המפרט שלו, משפר את היציבות ותמיכת הכלים.

## ראה גם:
- מאגר TOML הרשמי ב-GitHub & המפרט: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, ספריית ה-.NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
