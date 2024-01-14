---
title:                "C#: זיקוק מחרוזות"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

### נגיד לך סוד

הודות לטכנולוגיות המתקדמות של עיבוד שפת תכנות כמו סי שארפ, יש לנו יתרונות רבים לעבוד עם מחרוזות בצורה יעילה וקלה. אז למה כדאי לנו לבחור לחבר מחרוזות באופן יפהפה?

## כיצד לעשות את זה

### באמצעות תרגיל

תחילה, נתחיל עם הקוד הבא כדי לחבר את שני מחרוזות יחד:

```C#
string firstString = "שלום";
string secondString = "עולם";
string combinedString = firstString + " " + secondString;
Console.WriteLine(combinedString);
```
הפלט יהיה:

```
שלום עולם
```

ניתן גם להשתמש בפונקציה `string.Join()` כדי לחבר מספר מחרוזות ביחד בצורה נקייה יותר:

```C#
string[] stringList = {"אני", "אוהב", "לתכנת"};
string combinedString = string.Join(" ", stringList);
Console.WriteLine(combinedString);
```

הפלט יהיה:

```
אני אוהב לתכנת
```

## Deep Dive

### עומק יותר על חיבור מחרוזות

חיבור מחרוזות הוא פעולה בסיסית בשפת תכנות והיא יכולה להיות מאוד שימושית כאשר אנו צריכים ליצור מחרוזת חדשה מתוך מספר מחרוזות קיימות. בסיסית, חיבור מחרוזות משתמש באופרטור + כדי להחבר שתי מחרוזות יחד. אבל ניתן גם להשתמש בפונקציות כדי לבצע חיבור מבוסס עקרונות יותר מתקדמים, כגון `string.Concat()` ו- `string.Format()`.

תוכלו ללמוד עוד על חיבור מחרוזות ופונקציות נוספות במדריכים הבאים:

- [הסבר על `string.Concat()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [הסבר על `string.Join()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.join)
- [הסבר על `string.Format()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)
- [פונקציות נוספות לעבוד עם מחרוזות בסי שארפ](https://docs.microsoft