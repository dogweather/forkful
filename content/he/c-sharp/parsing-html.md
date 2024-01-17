---
title:                "פירוק HTML"
html_title:           "C#: פירוק HTML"
simple_title:         "פירוק HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

מה ולמה?
פריסת HTML היא תהליך שבו מתבצע ניתוח של קוד ה-HTML כדי להבין את מבנה הדף האינטרנט. הפריסה מאפשרת לפיתוחנים לקרוא ולהבין את הנתונים המופיעים בדף, ובכך לבנות יישומים מתקדמים יותר.

איך לעשות?
```C#
using System;
using System.Net;
using HtmlAgilityPack;

WebClient client = new WebClient(); // יצירת מופע של תכנית "לקוח"
string html = client.DownloadString("https://www.example.com"); // הורדת הדף כקוד-HTML
HtmlDocument doc = new HtmlDocument();
doc.LoadHtml(html); // פארסינג - גישה לאובייקטים של נתוני הדף
foreach(HtmlNode node in doc.DocumentNode.SelectNodes("//a")){ // מציאת כל הקישורים בדף
Console.WriteLine(node.GetAttributeValue("href", "")); // הדפסת כתובת הקישורים
}
```

דילוג לעומק:
הפריסה הייתה חלק בלתי נפרד מפיתוח דפי האינטרנט כבר מאז התחלתם. בעבר, דבר זה היה נעשה בעיקר על ידי ידניים, אך כיום ישנן כלים רבים המאפשרים פריסה אוטומטית של קוד ה-HTML. חלופות לכלים אלו כוללות את פיתוח ממשקי תכנותית כגון Selenium ו- Puppeteer, אשר מאפשרים פעולות נוספות על הדף כגון לחיצה על כפתורים ומילוי טפסים.

הסתכל גם:
אתר זה מציע כלים נוספים הקשורים לפריסת HTML על ידי שפות תכנות נוספות כמו Java, Python ו-JavaScript. ניתן גם לשתף את הקוד המתקדם יותר שלנו בקהילה כדי לקבל משוב ולהפוך אותו לטוב יותר.