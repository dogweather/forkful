---
title:                "C#: Аналізування HTML"
simple_title:         "Аналізування HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Чому
У програмуванні часто доводиться працювати з різними форматами даних, включаючи веб-сторінки. Парсинг HTML є одним зі способів отримати структуровані дані з веб-сторінки для подальшого використання у своїх проєктах.

## Як
Для початку, необхідно імпортувати простір імен ```System.Net.Http``` та ```HtmlAgilityPack```, які забезпечать необхідні класи для роботи з HTML. Далі можна використовувати метод ```GetAsync()``` для отримання HTML-коду веб-сторінки, а потім створити об'єкт типу ```HtmlDocument``` і використовувати метод ```LoadHtml()``` для завантаження коду. Після цього, можна здійснювати пошук елементів за допомогою методу ```SelectNodes()``` і обробляти отримані дані.

```C#
using System.Net.Http;
using HtmlAgilityPack;

// Виклик асинхронного методу для отримання HTML-коду
var client = new HttpClient();
var response = await client.GetAsync("https://example.com");
var responseString = await response.Content.ReadAsStringAsync();

// Створення об'єкта HtmlDocument та завантаження коду
var doc = new HtmlDocument();
doc.LoadHtml(responseString);

// Пошук елементів за тегом "a"
var links = doc.DocumentNode.SelectNodes("//a");

// Записування посилань у список
var linksList = new List<string>();
foreach (var link in links)
{
    linksList.Add(link.Attributes["href"].Value);
}

// Виведення результатів
Console.WriteLine("Посилання на сторінку:");
foreach (var link in linksList)
{
    Console.WriteLine(link);
}
```

В даному прикладі ми отримали всі посилання з веб-сторінки під тегом ```<a>``` і вивели їх у консоль. Таким чином, можна отримати інші дані з веб-сторінки, використовуючи різні методи та фільтри.

## Глибинний аналіз
Для більш детальної роботи з HTML-документами, можна використовувати більш складні методи з класу ```HtmlDocument``` для пошуку і обробки елементів за допомогою CSS-селекторів, збирання метаданих і багато іншого. Також, можна використовувати фреймворки, які спрощують парсинг HTML, наприклад, ```AngleSharp```, ```CsQuery``` або ```HtmlParser```.

## Дивись також
- [Ресурси для розвитку веб-скрейпінгу на C#](https://medium.com/@a_l_e_x_u_a/%D1%80%D0%B5%D1%81%D1%83%D1%80%D1%81%D0%B8-%D0%B4%D0%BB%D1%8F-%D1%80%D0%BE%D0%B7%D0%B2%D0%B8%D1%82%D0%BA%D1%83-%D0%B2%D0%B5%D0%B1-%D1%81%D0%BA%D1%80%D0%B5%D0%B9%D0%BF%D1%96%D0%BD%D0