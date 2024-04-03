---
date: 2024-01-20 17:38:52.781785-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4\
  \ \u05E4\u05E9\u05D5\u05D8 \u05DE\u05E2\u05D1\u05E8 \u05DE\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DC\u05E6\u05D5\
  \u05E8\u05DA \u05E2\u05E7\u05D1\u05D9\u05D5\u05EA, \u05D4\u05E9\u05D5\u05D5\u05D0\
  \u05D5\u05EA \u05E9\u05DC\u05D0 \u05EA\u05DC\u05D5\u05D9\u05D5\u05EA \u05E8\u05D9\
  \u05E9\u05D9\u05D5\u05EA, \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7\u05D9 \u05D8\u05E7\
  \u05E1\u05D8."
lastmod: '2024-03-13T22:44:39.321292-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8 \u05DE\u05E2\u05D1\u05E8 \u05DE\u05D0\u05D5\u05EA\u05D9\u05D5\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D5\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\
  \u05EA \u05E7\u05D8\u05E0\u05D5\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## מה ולמה?
המרת מחרוזת לאותיות קטנות זה פשוט מעבר מאותיות גדולות לאותיות קטנות. מתכנתים עושים את זה לצורך עקביות, השוואות שלא תלויות רישיות, וניתוחי טקסט.

## איך לעשות:
```C#
string original = "Shalom, World!";
string lowercased = original.ToLower();
Console.WriteLine(lowercased); 
// פלט: shalom, world!
```

עוד דוגמה:
```C#
string greeting = "ברוך הבא!";
string lower = greeting.ToLower();
Console.WriteLine(lower);
// פלט: ברוך הבא!
```

## צלילה עמוקה
בתחילת התכנות, רגישות לרישיות במחרוזות הייתה נפוצה. עם הזמן, פונקציות כמו `ToLower()` הפכו לסטנדרט להמרה של טקסטים. החלופות כוללות שימוש בפונקציות כמו `ToLowerInvariant()` שמבטיחות תוצאה עקבית ללא תלות בלוקליזציה. בנושא ביצועים, שימוש במתודות אלו יעיל ולרוב לא יהווה עול על המערכת, אבל זהירות צריכה להתקיים כאשר עובדים עם מחרוזות גדולות מאוד או בפעולות אחזור מרובות.

## ראה גם:
- [Microsoft Docs - ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- [Microsoft Docs - ToLowerInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-6.0)
- [Stack Overflow: When to use ToLowerInvariant over ToLower](https://stackoverflow.com/questions/6225808/string-tolower-and-string-tolowerinvariant)
