---
date: 2024-01-20 17:47:11.162966-07:00
description: "\u05D0\u05D9\u05DA \u05E2\u05D5\u05E9\u05D9\u05DD: \u05E7\u05D5\u05D3\
  \ C# \u05DC\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05E8\u05D0\u05D4 \u05DB\u05DB\u05D4\
  ."
lastmod: '2024-03-13T22:44:39.327733-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05D5\u05D3 C# \u05DC\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\
  \u05DA \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05E8\u05D0\u05D4\
  \ \u05DB\u05DB\u05D4."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## איך עושים:
קוד C# למציאת אורך של מחרוזת נראה ככה:
```C#
string myString = "שלום עולם!";
int stringLength = myString.Length;
Console.WriteLine(stringLength);  // פלט: 11
```
ערך `Length` מחזיר את מספר התווים.

## צלילה עמוקה:
אורך מחרוזת ב-C# מבוסס על תכונה של אובייקטים מסוג `String`. מאז C# נוצר ב-2000, זה היה כלי בסיסי. בשפות אחרות יש פונקציות כמו `strlen()` ב-C ו-C++. ב-C# זה פשוט יותר – אתה פשוט משתמש בפרופרטי `Length`.

מחרוזות ב-C# הן נציגות סדרה של תווים 'Char', כאשר כל תו מוצג על ידי יחידת קוד Unicode. הם אובייקטים immutable (לא ניתן לשנותם לאחר יצירה), כך שכאשר אנו משנים את המחרוזת, אנו בעצם יוצרים אחת חדשה.

פרט טכני נוסף הוא שלעיתים קרובות מחרוזות ארוכות מאוד יכולות לגרום לביצועים לא אופטימליים עקב צורך בריצה על כל התווים כדי לספור אותם. לכן, כאשר אפשר, מנסים לשמור על אורך ידוע מראש ולא לקרוא ל`Length` בלולאה או בפעולות חזרתיות.

## ראו גם:
- [Microsoft Docs: String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [Microsoft Docs: String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string)
- [Unicode Standard](https://home.unicode.org/)
- [Stack Overflow: Why are strings immutable?](https://stackoverflow.com/questions/2365272/why-net-string-is-immutable)
