---
title:                "C#: מיזוג מחרוזת לאותיות רישיות"
simple_title:         "מיזוג מחרוזת לאותיות רישיות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה:

להגדיל את הערך שנמצא בתוך מחרוזת היא פעולה מצוינת לכל מי שמעוניין לנהל ולנתח מחרוזות בתוך קוד תכנותי בשפת סי-שארף.

# איך לעשות:

שימו לב שפעולת "ToUpper" תופעל רק על התו הראשון שבין הסימנים ההפרדה. לכן עליכם לבצע המרה אחרת על כל תו שנמצא בכל מחרוזת. לדוגמה:

```C#
string str = "this is a string";
string result = "";
foreach (char c in str)
{
  result += c.ToString().ToUpper();
}

// output: "THIS IS A STRING"
```

# מעמקים:

ישנן פתרונות נוספים להגדלת מחרוזות, וכן שימוש בפונקציות מובנות יותר בשפת סי-שארף כמו "ToUpperInvariant", "ToUpperInvariant", ו-"TextInfo.ToTitleCase". כמו כן, כדאי לבדוק מהי ההשפעה של השימוש בתווים בעלי גודל אותו הבגרלת "IgnoreCase" או טיפולים נוספים במחרוזות.

# ראו גם:

- [תיעוד על אופציות הגדלת מחרוזות בשפת סי-שארף](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [מאמר בנושא טיפול במחרוזות בשפת סי-שארף](https://www.codeproject.com/Articles/404815/String-Handling-in-Csharp)