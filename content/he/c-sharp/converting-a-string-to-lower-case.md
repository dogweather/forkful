---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- he/c-sharp/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:52.781785-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

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
