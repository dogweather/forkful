---
title:                "Разбор HTML"
aliases:
- ru/c-sharp/parsing-html.md
date:                  2024-01-28T23:59:54.226366-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор HTML представляет собой извлечение информации из HTML-документов. Программисты делают это для программного взаимодействия с веб-контентом, сбора данных или автоматизации веб-взаимодействий.

## Как это делать:
Давайте воспользуемся популярной библиотекой .NET для разбора HTML: HtmlAgilityPack.

Сначала установите его через NuGet:
```shell
Install-Package HtmlAgilityPack
```

Затем загрузите HTML-документ и получите некоторые узлы:

```C#
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com");

        foreach (var node in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            Console.WriteLine($"Текст: {node.InnerText}, Ссылка: {node.Attributes["href"].Value}");
        }
    }
}
```
Приведенный выше фрагмент извлекает все теги якоря с атрибутом `href` и выводит их текст и ссылку.

Пример вывода может выглядеть так:

```
Текст: Главная, Ссылка: http://example.com/home
Текст: О нас, Ссылка: http://example.com/about
...
```

## Подробнее
HtmlAgilityPack (HAP) является ведущей библиотекой для разбора с начала 2000-х. Её ценят за гибкость и простоту использования, она тесно имитирует DOM в браузерах.

Альтернативы? Конечно. AngleSharp - это более новая библиотека с поддержкой асинхронности, которая более точно следует современным веб-стандартам. Для простых задач можно даже использовать регулярные выражения, но будьте осторожны - HTML не был создан для работы с регулярными выражениями. Это скорее костыльное решение.

С точки зрения реализации, HAP анализирует данный HTML в структуру, похожую на DOM, позволяя вам запросы и манипуляции с узлами с использованием XPath или LINQ. Библиотека достаточно надежна, чтобы обрабатывать некорректный HTML, что дает ей преимущество при скрапинге реальных, часто несовершенных веб-страниц.

## Смотрите также
- HtmlAgilityPack на GitHub: [https://github.com/zzzprojects/html-agility-pack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub и документация: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
- Статья о лучших практиках веб-скрапинга: (ссылка на авторитетный источник с рекомендациями и юридическими аспектами веб-скрапинга).
