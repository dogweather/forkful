---
title:                "ניתוח HTML"
date:                  2024-01-20T15:31:19.416110-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פרסור HTML הוא תהליך שבו אנחנו פותרים את הקוד של HTML לחתיכות נתונים שהמחשב יכול לנהל בקלות. פרוגרמרים עושים את זה כדי למשוך מידע ספציפי מדפי אינטרנט, למנוע שימוש ב-API, או כדי לבדוק את פעולות האתר.

## איך לעשות:
נבנה תוכנית פשוטה ב-C# שמפרסרת HTML באמצעות החבילה HtmlAgilityPack.

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

class HtmlParser
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com");

        var headers = doc.DocumentNode.SelectNodes("//h1");
        foreach (var header in headers)
        {
            Console.WriteLine(header.InnerText);
        }
    }
}
```

הפלט יהיה הטקסט של כותרות ה-H1 מהדף example.com.

## עומק הנושא:
בעבר, פרסור HTML היה מסובך יותר. רוב הכליות לא היו בשימוש, ופרוגרמרים היו נאלצים לפתח פיתרונות ייחודיים. היום, יש לנו חבילות כמו HtmlAgilityPack שמקלות עלינו את העבודה. זה לא האפשרות היחידה – ישנם כלים אחרים כמו אנגל'ה פרסר ו-CSQuery. הטריק הוא לבחור את הכלי המתאים לצורך הספציפי שלך: חזות, ביצועים, תמיכה ב-XPath ועוד. 

HtmlAgilityPack, למשל, טובה בפרסור מסמכים שאינם תקניים, מה שמאד שימושי בעולם אינטרנט אמיתי שלא תמיד נקי משגיאות.

## ראה גם:
- המסמך הרשמי של HtmlAgilityPack: https://html-agility-pack.net/
- דוקומנטציה של קבוצת תקינה של HTML (W3C): https://www.w3.org/TR/html52/
- XPath Tutorial: https://www.w3schools.com/xml/xpath_intro.asp
