---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:54.226366-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u043E\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F \u043F\u043E\u043F\u0443\u043B\u044F\
  \u0440\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u043E\
  \u0439 .NET \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 HTML:\
  \ HtmlAgilityPack. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\
  \u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0435\u0433\u043E \u0447\u0435\u0440\
  \u0435\u0437 NuGet."
lastmod: '2024-03-13T22:44:45.052235-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0432\u043E\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u0441\u044F \u043F\u043E\u043F\u0443\u043B\u044F\
  \u0440\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u043E\
  \u0439 .NET \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 HTML."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

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
