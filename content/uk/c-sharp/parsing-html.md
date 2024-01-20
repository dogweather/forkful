---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML це процес, через який ми можемо витягти корисну інформацію з HTML-документів. Програмісти роблять це, щоб аналізувати, маніпулювати або навіть виготовляти нові дані, що залежать від вмісту HTML.

## Як зробити:
Нижче подано приклад коду у C#, який показує, як парсити HTML за допомогою бібліотеки HtmlAgilityPack:

```C#
using HtmlAgilityPack;

private static void Main(string[] args)
{
    HtmlWeb web = new HtmlWeb();
    HtmlDocument document = web.Load("https://приклад.com");

    foreach (HtmlNode node in document.DocumentNode.SelectNodes("//а"))
    {
        string link = node.GetAttributeValue("href", string.Empty);
        Console.WriteLine("Посилання: " + link);
    }
}
```
У цьому прикладі ми отримуємо всі посилання (<a href="....") з HTML-сторінки і виводимо їх в консоль.

## Поглиблено:
1. Історичний контекст: HTML було створено 1991 року, але перший парсер HTML з'явився лише в 2000 році в формі HtmlAgilityPack.
2. Альтернативи: Незалежно від HtmlAgilityPack, є інші бібліотеки, такі як Fizzler або CsQuery.
3. Деталі реалізації: HtmlAgilityPack працює шляхом створення DOM (Document Object Model) з HTML-документа, що дозволяє вам обробляти HTML-елементи як об'єкти.

##Дивіться також:
Екосистема C# має чимало ресурсів для допомоги вам у поглиблений вивченні парсингу HTML. Ось декілька посилань, якими варто скористатись:
1. [Офіційна документація HtmlAgilityPack](https://html-agility-pack.net/)
2. [Вступ до парсингу HTML у C# на Medium](https://medium.com/@codenamejason/parsing-html-in-c-e9ddbbb96fdf)
3. [StackOverflow: альтернативи HtmlAgilityPack](https://stackoverflow.com/questions/2899275/c-sharp-html-parsing-library-similar-to-beautifulsoup)