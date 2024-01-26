---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:11.162966-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת זהו פעולה שמחזירה כמה תווים יש בה. תכנתים עושים את זה כדי לוודא קלט, לחתוך טקסט או לעבור על נתונים.

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
