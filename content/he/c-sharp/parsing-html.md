---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח HTML, באנגלית: Parsing HTML, הוא התהליך שבו קוראים ומפרשים את הקוד השיפועי של בניין אתרי HTML. מתכנתים עושים את זה כדי לחלץ, לשנות או לנתח את הנתונים מאתרי אינטרנט.

## איך ל:
```C#
using HtmlAgilityPack;
// טעינת דף HTML
var web = new HtmlWeb();
var doc = web.Load("http://yourwebsite.com");
// הגעה לאלמנט מסוים תחת div עם id מסוים
var Nodes= doc.DocumentNode.SelectNodes("//div[@id='yourdivid']//li");
// הגעה לתוכן הטקסט של האלמנט
foreach(var node in Nodes)
{
   Console.WriteLine(node.InnerText);
}
```
הקוד מעלה מטעין דף HTML, מחפש div עם id מסויים, ומטעין את הטקסטים של האלמנטים "li" תחתיו.

## צלילה עמוקה
ניתוח HTML הוא אכן מעט מסובך מאשר ניתוח טקסט פשוט, בזכות המבנה המשולב של HTML ברמות שונות. בעבר, המתכנתים יצרו קוד ניתוח באופן ידני, אך היום משפחת הספריות `HtmlAgilityPack` שיהלם את זיקת המשא ומתן ומניעת שגיאות.

אלטרנטיבות ל`HtmlAgilityPack` כוללות `CsQuery` ו`AngleSharp`, כאשר כל אחת מהן מספקת את יתרונות וחסרונותיה המיוחדים.

## ראו גם
למדע נוסף על בניית HTML משובצת ואופטימיזציה של קוד ניתוח, ראו את המקורות הבאים:
1. [מסמכי HtmlAgilityPack](https://html-agility-pack.net/)
2. [היכן לקרוא על AngleSharp](https://anglesharp.github.io/docs/)
3. [CSQuery ב- GitHub](https://github.com/jamietre/csquery)