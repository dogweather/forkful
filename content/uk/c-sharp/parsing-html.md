---
title:                "Аналіз HTML"
html_title:           "C#: Аналіз HTML"
simple_title:         "Аналіз HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Що & Чому?
Парсинг HTML - це процес витягування даних з HTML-коду веб-сторінки. Програмісти часто використовують його для отримання необхідної інформації зі сторінок, які немають готового API для доступу до даних. 

## Як зробити:
```C#
// Приклад коду для парсингу HTML-коду використовуючи бібліотеку HtmlAgilityPack
HtmlDocument document = new HtmlDocument();
document.Load("index.html");
HtmlNodeCollection paragraphs = document.DocumentElement.SelectNodes("//p");
foreach (HtmlNode paragraph in paragraphs)
{
    Console.WriteLine(paragraph.InnerText);
}
```

Вихід: Вітаємо на нашій сторінці! Будь ласка, ознайомтеся з нашими послугами.

## Глибше в деталі:
Спочатку парсинг HTML використовувався для розбору текстових документів, але з появою інтернету став популярним інструментом для витягування даних з веб-сторінок. Є інші способи доступу до даних, такі як розробка API або використання скрапінгу веб-сторінок, але парсинг дає можливість отримати необхідні дані без потреби висилати запити до сервера. У C# існує кілька бібліотек для парсингу HTML, таких як HtmlAgilityPack, AngleSharp та CsQuery.

## Дивись також:
- [HtmlAgilityPack офіційний сайт](https://html-agility-pack.net/)
- [Документація з HtmlAgilityPack в Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/htmlagilitypack?view=htmlagilitypack-1.11.37)