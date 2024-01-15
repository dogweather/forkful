---
title:                "חיפוש והחלפת טקסט"
html_title:           "C#: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מדוע

בפיתוח תוכניות ואפליקציות, טכניקת החיפוש וההחלפה של טקסט היא חיונית בכדי לשפר את היעילות ולפתור בעיות תכנותיות.

## איך לעשות זאת

לבצע חיפוש והחלפה של טקסט בשפת C# ניתן בעזרת פונקציות פשוטות כמו Replace() ו-ReplaceAll(). הנה דוגמאות לשימוש בהן:

```
// השתמש בפונקציה Replace() כדי להחליף את מופע המילה "משהו" במערך של טקסט
string[] textArr = { "משהו מרתק", "משהו פשוט", "משהו מעניין" };
foreach(string text in textArr)  
{  
    Console.WriteLine(text.Replace("משהו", "משהו אחר"));  
}

// פלט: "משהו אחר מרתק", "משהו אחר פשוט", "משהו אחר מעניין"

// השתמש בפונקציה ReplaceAll() כדי להחליף את כל מופעי הגרשיים בטקסט לסימן המחרוזת מתאימה
string text = "אני אוהב 'עוגת תפוחים'";
Console.WriteLine(text.ReplaceAll("\'", "\""));

// פלט: "אני אוהב עוגת תפוחים"
```

## בירור עמוק

פונקציות חיפוש והחלפה נמצאות במחלקת String בשפת C# ומאפשרות למצוא ולהחליף באופן נוח ומהיר טקסט במחרוזת. כמו כן, ניתן לעבוד עם פונקציות נוספות כגון ReplaceFirst() ו- ReplaceLast() לפעולות החלפה מפורטות יותר.

## ראה גם

למידע נוסף על חיפוש והחלפה של טקסט בשפת C#, הצטרף לקהילת הפורומים וקרא את המדריכים המקיפים שלהם:

- [חיפוש והחלפת טקסט בשפת C#](https://forum.devshed.com/c-programming-42/search-replace-text-c-945006.html)
- [מדריך לפונקציות חיפוש והחלפה בשפת C#](https://www.tutorialspoint.com/csharp/csharp_string_replace.htm)