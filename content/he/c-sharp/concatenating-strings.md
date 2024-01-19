---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת מחרוזות היא התהליך של מיזוג שני או יותר מחרוזות למחרוזת אחת. מתכנתים מבצעים התמרה זו כדי ליצור פלט המכיל מידע שונה בתוך מחרוזת אחת.

## איך לעשות:

הנה כמה דוגמאות למיזוג מחרוזות ב-C#:

```C#
// מזג על ידי שימוש באופרטור '+'
string welcome = "שלום " + "עולם";
Console.WriteLine(welcome); // תוצאה "שלום עולם"

// מזג על ידי שימוש במתודת 'String.Concat'
string wish = String.Concat("מזל ", "טוב");
Console.WriteLine(wish); // תוצאה "מזל טוב"
```

## צלילה עמוקה:

בהקשר ההיסטורי, מתכנתים השתמשו ב '+' כדי למזג מחרוזות, אך זה אינו מיטבי מבחינה ביצועית. על כן, C# הווה את 'String.Concat' וכמה אלטרנטיבות אחרות.

האלטרנטיבות ל'+' כלולות 'String.Concat', 'String.Join', ו'StringBuilder.Append'.

ב'+' ו'String.Concat', C# מייצר מחרוזת חדשה בכל פעם שמתרחש מיזוג. בניגוד לכך, 'StringBuilder' נוצר פעם אחת ואז מוסיף מחרוזות, שהופך אותו למהיר יותר בביצועים.

## ראה גם:

1. [MSDN String.Concat](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=net-5.0)
2. [MSDN String.Join](https://docs.microsoft.com/en-us/dotnet/api/system.string.join?view=net-5.0)
3. [MSDN StringBuilder.Append](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder.append?view=net-5.0)