---
title:                "שימוש בביטויים רגולריים"
html_title:           "C#: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

פקודות ביטויים רגילים הן כלי חזק וחיוני לניתוח ועיבוד טקסט בתוכניות שכתובות ב-C#. הן מאפשרות למתכנתים לבצע פעולות מורכבות יותר משימוש בפקודות פשוטות.

## איך לעשות את זה

```C#
// מכיל מחרוזת של כתב אותיות באנגלית
string text = "Hello, World!";

// בחריזה להתאים ל- "Hello"
Match match = Regex.Match(text, @"Hello");

// הדפס התאמה
Console.WriteLine(match.Value); // ידפיס "Hello"
```

```C#
// מחשבים ציונים מדויקים מתוך מחרוזת של מספרים
string numbers = "12 + 3 - 5 * 2";

// השתמש בתבנית לחיפוש עבור מספרים וסימנים בעזרת פקודות ביטויים רגילים
MatchCollection matches = Regex.Matches(numbers, @"\d+|[-\+\*]");

// ללולאה על כל התאמה כדי להדפיס תוצאה
foreach (Match match in matches)
{
    Console.WriteLine(match.Value); // ידפיס "12", "+", "3", "-", "5", "*"
}
```

## חפירה עמוקה

פקודות ביטויים רגילים מאפשרות לנו ליצור תבניות מורכבות של טקסט ולחפש ולעבד אותו בצורה יעילה. הן נותנות לנו את היכולת לטפל במגוון רחב של מסרים והן יכולות ליישם תנאים מחייבים כדי להעביר תהליך פעולה או לקבוע תנאים תוך שימוש בפקודות ביטויים רגילים. כמו כן, הן יכולות לשמור על קשיחות ובטחון בתוך עסקאות טקסטואליות סודיות.

## ראו גם

- [שימושים נפוצים לפקודות ביטויים רגילים ב-C#](https://www.microsoft.com/he-il/download/details.aspx?id=49986)
- [דוגמאות לכתיבת פקודות ביטויים רגילים](https://docs.microsoft.com/he-il/dotnet/standard/base-types/regular-expression-examples)