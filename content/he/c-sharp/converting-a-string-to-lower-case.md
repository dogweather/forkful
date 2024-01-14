---
title:    "C#: עיבוד תווים: המרת מחרוזת לאותיות קטנות"
keywords: ["C#"]
---

{{< edit_this_page >}}

# למה

כשמתחילים ללמוד תכנות, מושגים כמו "lower case" ו-"upper case" יכולים להיות מבלבלים. אז מדוע כדאי לדעת איך להמיר מחרוזת ל-"lower case"? בכדי להפוך את הקוד שלנו ליותר קריא וידידותי, עלינו ללמוד את כל הטריקים והכלים הפשוטים.

# איך לעשות את זה?

```C#
string str = "HELLO WORLD";
string lowerCase = str.ToLower();
Console.WriteLine(lowerCase);
//Output: hello world
```

כפי שאתם רואים בדוגמה למעלה, כדי להמיר מחרוזת ל-"lower case" ישנם שני אפשרויות. הראשונה היא להשתמש במתודה של `ToLower()` על המחרוזת שרוצים להמיר. השנייה היא לעבור על כל אות במחרוזת ולהמיר אותה לעזרת הפונקציה `Char.ToLower()`.

# עיון מעמיק

כאשר משתמשים במתודה `ToLower()` או בפונקציה `Char.ToLower()`, המחרוזת ממירה את כל האותיות ל-"lower case" תיבת התצוגה הנוכחית של המחשב. זה יכול להיות שונה בתצוגת הטקסטים בשפות שונות. לדוגמה, אם אתם עובדים עם שפה עברית, מתודת `ToLower()` תמיר את האותיות ל-"lower case" באופן המתאים לאותיות העבריות.

# ראו גם

- [מדריך על שפת C# של Microsoft](https://docs.microsoft.com/he-il/dotnet/csharp/)
- [מדריך על מחרוזות ב-C#](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/strings/)
- [פונקציות חשובות ב-C#](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/types/important-built-in-types)