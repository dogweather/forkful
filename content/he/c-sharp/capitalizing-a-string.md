---
title:                "C#: כתיבת מחרוזת באותיות גדולות"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות לכך שבתכנות בשפת C# ייתכן שנאלץ לשנות את האות הראשונה של מחרוזת לאות גדולה. אחת הסיבות הנפוצות היא לצורך הצגת מידע בצורה מלאה ומותאמת לקריאה למשתמשים.

## איך לעשות זאת

כדי להחליף את האות הראשונה של מחרוזת לאות גדולה בשפת C# ניתן להשתמש בפונקציה "ToUpper". הנה דוגמה קוד להמחשת השימוש בפונקציה זו:

```C#
string name = "john";
Console.WriteLine(name.ToUpper());
```

תוצאה:
```C#
JOHN
```

במקרים בהם ברצוננו להחליף את האות הראשונה של כל מילה במחרוזת, ניתן להשתמש גם בפונקציה "ToTitleCase". הנה דוגמה לשימוש בפונקציה זו:

```C#
string sentence = "hello world";
Console.WriteLine(CultureInfo.CurrentCulture.TextInfo.ToTitleCase(sentence));
```

תוצאה:
```C#
Hello World
```

## חקירה מעמיקה

אם נרצה לעשות עבודה נוספת על המחרוזת, ניתן להשתמש במספר פונקציות נוספות כמו "Insert", "Replace" ו-"Remove". למשל, נהיה יכולים להחליף את התו הראשון של מחרוזת בעזרת הפונקציה "Replace" ולהוסיף תו או מחרוזת נוספת בתחילתה באמצעות הפונקציה "Insert".

## ראה גם

- [מדריך לפורמטים של מחרוזות בשפת C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types)
- [תיעוד פונקציות המחרוזות בשפת C#](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netframework-4.7.2)