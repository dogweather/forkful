---
title:                "עיבוד HTML"
html_title:           "C#: עיבוד HTML"
simple_title:         "עיבוד HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

סוג שני

## למה

למה לקוראי המאמר יהיה עניין ללמוד איך לנתח קובץ HTML בשפת C#.

## איך לעשות זאת

הפעולה של נתיחת קובץ HTML בשפת C# יכולה להיות מעניינת ומאתגרת. כדי לעזור לקוראי המאמר להבין טוב יותר את הפעולה, הנה כמה דוגמאות לקוד ולפלט בתוך חלקי קוד " ```C# ... ```".

- הסתיוורי לגבי דוגמאות:
```C#
// דוגמא לקוד C# לנתח תגית div בתוך קובץ HTML
string html = "<div><p>זהו טקסט תוך תגית div</p></div>";

HtmlDocument doc = new HtmlDocument();
doc.LoadHtml(html);

HtmlNode divNode = doc.DocumentNode.SelectSingleNode("//div");
HtmlNode paragraphNode = divNode.SelectSingleNode("//p");

// הדפסת הטקסט שמופיע בתוך התגית p
Console.WriteLine(paragraphNode.InnerText); // תצוגת הטקסט המצוין בתוך הטקס המוגדר כטקסט רגיל

// דוגמא נוספת לקוד C# לנתח אתר HTML כולו
string websiteUrl = "https://www.example.com";
HtmlWeb web = new HtmlWeb();
HtmlDocument doc = web.Load(websiteUrl);

HtmlNodeCollection divNodes = doc.DocumentNode.SelectNodes("//div");

foreach (HtmlNode divNode in divNodes)
{
    // הדפסת כל הטקסטים שמופיעים בתוך כל תגיות div
    Console.WriteLine(divNode.InnerText);
}
```

## חפירה מעמיקה

נתיחת קובץ HTML היא פעולה בסיסית שמאפשרת לנו לפענח ולקרוא את מבנה התוכן של קבוצת תגיות HTML. כמו כן, באמצעות נתיחת הקובץ אנו יכולים ליצור טכניקות חכמות יותר לבניית אפליקציות ואתרים. למידע נוסף על השימוש בכלים נוספים לנתתיחת HTML בשפת C# ניתן לגלות במקורות המידע המצויינים בלינקים המצורפים לסוף המאמר.

## ראו גם

לפני כתיבת הקוד, כדאי להיכנס לאשכול המתאים בפורום של קהילת הפיתוחים על מנת