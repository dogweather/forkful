---
title:    "C#: חיפוש והחלפת טקסט"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

חיפוש והחלפת טקסט הוא כלי חיוני בתכנות בשפת C#. באמצעותו, אפשר לשנות את התוכן של מחרוזות, קבצים ועוד, לפי הצורך שלנו. אם אתם מעוניינים לפתח יכולות תכנות מתקדמות בשפת C#, מנתקדים אכן לצורך כידוע לתרגל ולהכיר את כלי החיפוש וההחלפה שמציעה השפה.

## איך לעשות את זה

```C#
// דוגמה לחיפוש והחלפת טקסט במחרוזת יחידה
string sentence = "שלום לכולם!";
string newSentence = sentence.Replace("שלום", "היי");
Console.WriteLine(newSentence);
// פלט: היי לכולם!
```

```C#
// דוגמה לחיפוש והחלפת טקסט בקובץ
string filePath = @"C:\Users\User\Documents\file.txt";
string fileContent = File.ReadAllText(filePath); // קריאת תוכן הקובץ למחרוזת
fileContent = fileContent.Replace("בכבוד", "בהערכה");
File.WriteAllText(filePath, fileContent); // כתיבת המחרוזת המעודכנת לקובץ המקורי
```

## העברה עמוקה

בנוסף לחיפוש והחלפת טקסט במחרוזות וקבצים, תוכלו גם להשתמש בכלי זה כדי לעבור על מבני נתונים שונים ולשנות אותם לפי הצורך. כמו כן, אפשר גם להשתמש בתנאיים כדי להחליף טקסט רק עבור מקרים מסוימים.

## ראו גם

- [חיפוש והחלפת טקסט בשפת C# - תיעוד מראש](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/string/string-replace)
- [דוגמה מעניינת לחיפוש והחלפת טקסט בשפת C#](https://www.c-sharpcorner.com/blogs/find-and-replace-strings-in-c-sharp1)