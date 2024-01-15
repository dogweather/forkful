---
title:                "שימוש באותיות גדולות במחרוזת"
html_title:           "C#: שימוש באותיות גדולות במחרוזת"
simple_title:         "שימוש באותיות גדולות במחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
משתמשים בפונקציית הקפטלייז כדי להמיר את האותיות במחרוזת מינוסקולס לתחילת מילים מינוסקולס. זה יכול להיות שימושי כאשר מעורבים עם תוכניות אחרות הדורשות פורמט של מילים עם אותיות גדולות בתחילתן.

## איך לעשות זאת
```C#
// פונקציית הקפטלייז מקבלת מחרוזת
string input = "hello world";

// ומחזירה את המחרוזת המקופסת
string output = input.ToUpper();

// נוצרת המחרוזת "HELLO WORLD"
```

```C#
// ניתן גם להשתמש בפונקציית הקפטלייז עם מחרוזת של מחרוזות
string input = "one two three";
string[] words = input.Split();

// נוצרת מערך של מחרוזות
// words = {"one", "two", "three"}

// משתמשים בלולאה כדי לעבור על כל מחרוזת
foreach (string word in words)
{
    // קופצים לסוף הלולאה אם המחרוזת ריקה
    if (string.IsNullOrWhiteSpace(word))
    {
        continue;
    }

    // אות התחילה של המחרוזת מתוחפת לקפטלייז
    string firstLetter = word[0].ToString().ToUpper();

    // פסקה של המחרוזת ללא האות הראשונה
    string restOfWord = word.Substring(1);

    // יוצאים עם המחרוזת המתוחפת
    string capitalizedWord = firstLetter + restOfWord;
    Console.WriteLine(capitalizedWord);
}

// פלט: One Two Three
```

## צלילה עמוקה
בגירסת הקפטלייז הנוכחית של סי שארפ, ניתן לשנות את המתווה של פונקציית הקפטלייז. למרבה המזל, ברירת המחדל היא לשנות רק את הסימן המופרד בין המילים. אבל ניתן גם לשנות את הכיוון של הפונקציה כדי להפוך את האות הראשונה של כל מילה לקפטלייז בלי לחלץ אותה ולמעשה לבצע את אותו דבר בצורה יעילה יותר.

## ראו גם
- [רשומה לקפטלייז בפונקצי