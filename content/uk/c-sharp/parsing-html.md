---
title:                "Розбір html"
html_title:           "C#: Розбір html"
simple_title:         "Розбір html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Розбір HTML є важливою задачею в програмуванні, оскільки дозволяє отримувати та обробляти дані з веб-сторінок. Це дозволяє створювати потужні додатки, які працюють з інформацією з Інтернету.

## Як це зробити

```C#
using System;
using System.Net;
using System.IO;
using HtmlAgilityPack;

// Завантаження HTML з веб-сторінки
string url = "https://www.example.com";
HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url);
HttpWebResponse response = (HttpWebResponse)request.GetResponse();

// Читання відповіді із потоку
StreamReader reader = new StreamReader(response.GetResponseStream());
string html = reader.ReadToEnd();

// Створення HTML-синтаксичного дерева
HtmlDocument doc = new HtmlDocument();
doc.LoadHtml(html);

// Знаходження тега <h1> та виведення його вмісту
var heading = doc.DocumentNode.SelectSingleNode("//h1");
Console.WriteLine(heading.InnerText); // Виведе "Українська версія"
```
Приклад виводу:

```
Українська версія
```

## Розглиблення

Розбір HTML - це процес аналізування вихідного коду веб-сторінки за допомогою програмного забезпечення. Після завантаження сторінки, весь HTML-код розбивається на окремі частини, які можна легко зчитувати та обробляти. Це дозволяє отримувати потрібні дані з Інтернету та використовувати їх у своїх програмах. Найпоширенішою бібліотекою для роботи з розбором HTML в C# є `HtmlAgilityPack`, яка дозволяє маніпулювати HTML-синтаксичним деревом зручним способом.

## Дивись також

- [HtmlAgilityPack документація](https://html-agility-pack.net/documentation)
- [Розділ бібліотеки HTMLAgilityPack на GitHub](https://github.com/zzzprojects/html-agility-pack)