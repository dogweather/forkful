---
title:                "C#: המרת מחרוזת לאותיות קטנות"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

למה צריך להמיר מחרוזת לאותיות קטנות?

## איך לבצע את התהליך

בכדי להמיר מחרוזת לאותיות קטנות בשפת סי שארפ, ניתן להשתמש בפעולת הפילוט המובנית ToString המאפשרת המרת נתונים לסוגים שונים. כדי להמיר מחרוזת לאותיות קטנות, נא להשתמש בפעולת הממשק ToLower הקיימת במחלקת String. נוכל להשתמש גם בפעולה ToLowerInvariant להמרת האותיות לתחתית מינוחית.

```C#
string myString = "HELLO WORLD";
string newString = myString.ToLower();

Console.WriteLine(newString); // Output: hello world
```

## לחקור עומק

מה אם ברצוננו להמיר רק את חלק מהמחרוזת לאותיות קטנות? במקרה כזה, ניתן להשתמש בפעולת Substring כדי לחלק את המחרוזת לחלקים ולהמיר רק את החלק הרלוונטי לאותיות קטנות.

```C#
string myString = "HELLO WORLD";

string firstPart = myString.Substring(0, 5); // "HELLO"
string secondPart = myString.Substring(5); // " WORLD"

string newString = firstPart + secondPart.ToLower();
Console.WriteLine(newString); // Output: Hello world
```

## ראה גם

- מדריך לתחביר הבסיסי של סי שארפ (https://www.codecademy.com/learn/learn-c-sharp)
- הממשק String במסמך הרשמי של מייקרוסופט (https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- דוגמאות ופתרונות לתרגילים בנושא מחרוזות ב-C# (https://www.tutorialspoint.com/csharp/csharp_strings.htm)