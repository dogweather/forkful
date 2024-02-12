---
title:                "פיענוח HTML"
aliases:
- /he/c-sharp/parsing-html.md
date:                  2024-02-03T19:12:16.458692-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ניתוח HTML בתכנות כולל את הניתוח של מבנה מסמך HTML, מה שמאפשר לך לחלץ, לשנות ולהתערב בתוכניו באופן תוכניתי. מתכנתים עושים זאת כדי לאוטמט גירידת אתרים, חילוץ נתונים או אף לשנות דפי אינטרנט או מסמכי HTML באופן דינמי ליישומים שונים, מה שהופך את זה לכישור חיוני בפיתוח אתרים, ניתוח נתונים, ותרחישי בדיקות אוטומטיות.

## איך לעשות:

למרות ש-NET מספקת תמיכה בסיסית בעבודה עם HTML, כמו למשל `HttpClient` לשליפת דפי אינטרנט, חסרה בה גרזנת HTML מובנית ומקיפה. לכן, רוב מפתחי C# מפנים פניה לספריות צד שלישי פופולריות כמו HtmlAgilityPack או AngleSharp ליכולת ניתוח HTML עמידה. שתי הספריות מאפשרות שאילתות, שינוי ועיבור קל של DOM ה-HTML.

### שימוש ב-HtmlAgilityPack

1. **התקנת HtmlAgilityPack**: ראשית, הוסף את החבילה של HtmlAgilityPack לפרויקט שלך דרך NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **דוגמת קוד**: לנתח מחרוזת HTML, ולחלץ את הכותרות של כל אלמנטי `<h1>`.

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Title 1</h1>
                             <h1>Title 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **פלט לדוגמא:**
   ```
   Title 1
   Title 2
   ```

### שימוש ב-AngleSharp

1. **התקנת AngleSharp**: הוסף את ספריית AngleSharp לפרויקט שלך דרך NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **דוגמת קוד**: טעינת מסמך HTML ושאילתא של אלמנטי `div` עם מחלקה ספציפית.

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Item 1</div><div class='item'>Item 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **פלט לדוגמא:**
   ```
   Item 1
   Item 2
   ```

HtmlAgilityPack ו-AngleSharp הם כלים עוצמתיים לניתוח HTML, אך הבחירה ביניהם עשויה להיות תלויה בדרישות הפרויקט הספציפיות, שיקולי ביצועים, או העדפה אישית בעיצוב ה-API.
