---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:09.897557-07:00
description: "\u042F\u043A: \u0425\u043E\u0447\u0430 .NET \u043D\u0430\u0434\u0430\
  \u0454 \u0431\u0430\u0437\u043E\u0432\u0443 \u043F\u0456\u0434\u0442\u0440\u0438\
  \u043C\u043A\u0443 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ HTML, \u043D\u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, `HttpClient` \u0434\
  \u043B\u044F \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0432\u0435\
  \u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043E\u043A, \u0432\u0456\u043D \u043D\
  \u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\
  \u0433\u043E, \u0432\u0441\u0435\u043E\u0441\u044F\u0436\u043D\u043E\u0433\u043E\
  \ HTML-\u2026"
lastmod: '2024-03-13T22:44:49.285083-06:00'
model: gpt-4-0125-preview
summary: "\u0425\u043E\u0447\u0430 .NET \u043D\u0430\u0434\u0430\u0454 \u0431\u0430\
  \u0437\u043E\u0432\u0443 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0443\
  \ \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 HTML, \u043D\u0430\
  \u043F\u0440\u0438\u043A\u043B\u0430\u0434, `HttpClient` \u0434\u043B\u044F \u043E\
  \u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0432\u0435\u0431-\u0441\u0442\
  \u043E\u0440\u0456\u043D\u043E\u043A, \u0432\u0456\u043D \u043D\u0435 \u043C\u0430\
  \u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0433\u043E, \u0432\
  \u0441\u0435\u043E\u0441\u044F\u0436\u043D\u043E\u0433\u043E HTML-\u043F\u0430\u0440\
  \u0441\u0435\u0440\u0430."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

## Як:
Хоча .NET надає базову підтримку для роботи з HTML, наприклад, `HttpClient` для отримання веб-сторінок, він не має вбудованого, всеосяжного HTML-парсера. Тому більшість розробників на C# звертаються до популярних сторонніх бібліотек, як-от HtmlAgilityPack або AngleSharp, для надійних можливостей синтаксичного аналізу HTML. Обидві бібліотеки дозволяють легко виконувати запити, маніпуляції та обходження DOM HTML.

### Використання HtmlAgilityPack
1. **Встановіть HtmlAgilityPack**: Спочатку додайте пакет HtmlAgilityPack до свого проекту через NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Приклад коду**: Розберіть HTML-рядок та витягніть заголовки всіх елементів `<h1>`.

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
                             <h1>Заголовок 1</h1>
                             <h1>Заголовок 2</h1>
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

   **Приклад результату:**
   ```
   Заголовок 1
   Заголовок 2
   ```

### Використання AngleSharp
1. **Встановіть AngleSharp**: Додайте бібліотеку AngleSharp до свого проекту через NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **Приклад коду**: Завантажте HTML-документ і запитайте `div` елементи з певним класом.

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
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Елемент 1</div><div class='item'>Елемент 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **Приклад результату:**
   ```
   Елемент 1
   Елемент 2
   ```

Як HtmlAgilityPack, так і AngleSharp є потужними інструментами для синтаксичного аналізу HTML, але ваш вибір між ними може залежати від конкретних вимог проекту, міркувань щодо продуктивності або особистих переваг у дизайні API.
