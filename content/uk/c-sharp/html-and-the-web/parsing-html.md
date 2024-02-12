---
title:                "Аналіз HTML"
aliases:
- /uk/c-sharp/parsing-html/
date:                  2024-02-03T19:12:09.897557-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
