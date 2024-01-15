---
title:                "שרשור מחרוזות"
html_title:           "C#: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# למה
בעולם התכנות, קיבוץ מחרוזות הוא כלי חשוב ושימושי שעוזר לנו לבנות מחרוזות מרוכבות יותר באופן יעיל. זה יכול להיות מועיל למתכנתים שרוצים לשמור על קוד קצר ותכנותי.

# איך לעשות זאת
הנה מספר דוגמאות של קיבוץ מחרוזות בשפת סי שארפ. הקוד והפלט מוצגים תחת בלוק "```C# ... ```"

### דוגמא 1:
```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```
פלט: John Doe

### דוגמא 2:
```C#
string greeting = "Hello";
string name = "Sarah";
string message = greeting + ", " + name + "!";
Console.WriteLine(message);
```
פלט: Hello, Sarah!

כפי שניתן לראות מהדוגמאות, ניתן להשתמש בסימן חיבור (+) כדי לקבץ מחרוזות יחד וליצור מחרוזות חדשות באופן תכנותי. ניתן גם להשתמש בפעולות אחרות כמו "string.Format" ו"StringBuilder.Append" כדי לקבץ מחרוזות בצורה יותר מתקדמת. 

# לחקור עומק יותר
קיבוץ מחרוזות הוא פעולה שנדרשת לעיבוד מחרוזת בנות או ליצירת מחרוזת חדשה. השימוש התכונות של הסימן הקיבוץ (+) יכול להיות מקור לבאגים בקוד ועל כן חשוב להיות זהירים בשימוש בו. כמו כן, ישנם בעיות בפרצוף בקיבוץ מחרוזות שמחייבים שימוש בפעולות תכנותיות יותר מתקדמות כגון מחלקת "StringBuilder".

# ראו גם
- [מדריך לקיבוץ מחרוזות בשפת סי שארפ](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [מאמר מפורט יותר על קיבוץ מחרוזות בסי שארפ](https://www.c-sharpcorner.com/blogs/string-concatination-in-c-sharp-programming1)