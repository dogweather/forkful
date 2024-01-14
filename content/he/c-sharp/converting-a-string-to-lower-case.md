---
title:                "C#: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה

למה לחשוב על המרה של מחרוזת לאותיות קטנות? יתרונותיו הניכרים בהמשך המאמר ישוו לך את הטרח ללמוד על פונקציות מועילות כמו .ToLower() בשפת תכנות C#.

# כיצד לבצע

הגעת למקום הנכון אם אתה מעוניין ללמוד כיצד להמיר מחרוזת לאותיות קטנות בשפת תכנות C#. נקדם לכוונה ונצג דוגמאות קוד כדי להבין טוב יותר.

```C#
string phrase = "שלום לעולם!";
string lowerPhrase = phrase.ToLower();
Console.WriteLine(lowerPhrase);
```

בתוצאה תקבל:

```
שלום לעולם!
```

נוכל לראות שהמחרוזת המקורית נמצאת עכשיו באותיות קטנות. אם נספק מחרוזת עם אותיות קפיטליות, הפונקציה .ToLower() תבצע את הפעולה הנדרשת ותחזיר את המחרוזת באותיות קטנות.

# טיפול מעמיק

השתמשנו כאן בפונקציה פנימית של שפת תכנות C#, אבל כמו רוב הפונקציות הפנימיות, ניתן לכתוב גם פונקציה עצמאית להביצעת המשימה. הפונקציה הבאה תחלופה את כל האותיות הגדולות במחרוזת לאותיות קטנות:

```C#
public string ConvertToLower(string input)
{
    string output = "";
    foreach (char letter in input)
    {
        output += char.ToLower(letter);
    }
    return output;
}
```

אם נרצה, נוכל גם להפוך את הפונקציה לסטטי כדי שנוכל לקרוא לה תוך טעינת הקלאס שלנו.

```C#
public static class StringConverter
{
    public static string ConvertToLower(string input)
    {
        string output = "";
        foreach (char letter in input)
        {
            output += char.ToLower(letter);
        }
        return output;
    }
}
```

עכשיו נוכל להשתמש בקלות בפונקציה הסטטית כאשר נרצה להמיר מחרוזת לאותיות קטנות:

```C#
string phrase = "שלום לעולם!";
string lowerPhrase = StringConverter.ConvertToLower(phrase);
Console.WriteLine(lowerPhrase);
```

כ