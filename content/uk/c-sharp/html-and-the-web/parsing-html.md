---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:09.897557-07:00
description: "\u0421\u0438\u043D\u0442\u0430\u043A\u0441\u0438\u0447\u043D\u0438\u0439\
  \ \u0430\u043D\u0430\u043B\u0456\u0437 HTML \u0443 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0443\u0432\u0430\u043D\u043D\u0456 \u0432\u043A\u043B\u044E\u0447\u0430\
  \u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438 HTML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0430,\
  \ \u0449\u043E \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C\
  \ \u0432\u0438\u043B\u0443\u0447\u0430\u0442\u0438, \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u0442\u0430 \u0432\u0437\u0430\u0454\
  \u043C\u043E\u0434\u0456\u044F\u0442\u0438 \u0437 \u0439\u043E\u0433\u043E \u0432\
  \u043C\u0456\u0441\u0442\u043E\u043C\u2026"
lastmod: '2024-03-11T00:14:23.142085-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0438\u043D\u0442\u0430\u043A\u0441\u0438\u0447\u043D\u0438\u0439\
  \ \u0430\u043D\u0430\u043B\u0456\u0437 HTML \u0443 \u043F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0443\u0432\u0430\u043D\u043D\u0456 \u0432\u043A\u043B\u044E\u0447\u0430\
  \u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438 HTML-\u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u0430,\
  \ \u0449\u043E \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C\
  \ \u0432\u0438\u043B\u0443\u0447\u0430\u0442\u0438, \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u0442\u0430 \u0432\u0437\u0430\u0454\
  \u043C\u043E\u0434\u0456\u044F\u0442\u0438 \u0437 \u0439\u043E\u0433\u043E \u0432\
  \u043C\u0456\u0441\u0442\u043E\u043C\u2026"
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
---

{{< edit_this_page >}}

## Що і чому?

Синтаксичний аналіз HTML у програмуванні включає аналіз структури HTML-документа, що дозволяє вам вилучати, маніпулювати та взаємодіяти з його вмістом програмно. Програмісти роблять це, щоб автоматизувати веб-скрапінг, екстракцію даних або навіть динамічно модифікувати веб-сторінки або HTML-документи для різних застосувань, що робить це суттєвою навичкою в розробці вебу, аналізі даних і сценаріях автоматизованого тестування.

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
