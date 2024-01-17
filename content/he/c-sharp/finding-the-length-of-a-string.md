---
title:                "מציאת אורך של מחרוזת"
html_title:           "C#: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

"מציאת אורך של מחרוזת" היא תופעה נפוצה בתכנות שכולנו מכירים. זהו פשוט את מספר התווים שמכילה מחרוזת. תכניות מחשב צריכות להכיר את אורך המחרוזת הנתונה כדי לדעת איך לעבד אותה כראוי.

## איך לעשות זאת:

```c#
string myString = "שלום עולם";
int length = myString.Length;
Console.WriteLine($"אורך המחרוזת שלי הוא: {length}");
```

##### פלט:

`אורך המחרוזת שלי הוא: 9`

בדוגמה שלעיל, אנו יוצרים משתנה מסוג "מחרוזת" ומשתמשים בפעולת "Length" כדי למצוא את האורך שלו. כתוצאה מכך, אנו מקבלים כפלט את אורך המחרוזת באמצעות פעולת הדפסה.

## חפירה עמוקה:

(1) כבר מאז תחילת התכנות, מציאת אורך של מחרוזת היא חלק מהבסיס של התכנות שכולנו משתמשים בו כמעט בכל יום. (2) בנוסף לפעולת "Length", ישנן גם אפשרויות נוספות כמו שימוש במשתנים ופעולות נוספים להתמודדות עם מחרוזות מסוימות בצורה יעילה יותר. (3) במידה והתקן הפיתוח המשתמש ברכבת המילים "רשומת אורך" כדי לשמור את אורך המחרוזת ולא משתמש בפעולת "Length", יימצאו תוצאות טובות יותר בביצועי מחשב.

## לרשומת ראות גם:

- [String.Length פעולה על MSDN](https://msdn.microsoft.com/en-us/library/system.string.length(v=vs.110).aspx)
- [מציאת אורך של מחרוזת בשפת C++](https://www.studytonight.com/cpp/string-length)
- [מדריך למתחילים בנושא מחרוזות בשפת C#](http://www.c-sharpcorner.com/UploadFile/m_m_k/string01292006130034PM/string.aspx)