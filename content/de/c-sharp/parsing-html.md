---
title:                "HTML parsen"
date:                  2024-01-20T15:30:41.030612-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bedeutet, den Code einer Webseite zu analysieren und zu interpretieren, um spezifische Inhalte und Strukturen zu extrahieren. Programmierer tun das, um Daten zu sammeln, Inhalte zu scrapen oder die Struktur einer Webseite programmatisch zu manipulieren.

## So geht's:
C# bietet mehrere Wege, um HTML zu parsen. Eine beliebte Bibliothek ist `HtmlAgilityPack`. Hier ist ein kurzes Beispiel, wie man es verwendet:

```C#
using HtmlAgilityPack;
using System;

class HtmlParser
{
    static void Main()
    {
        HtmlWeb web = new HtmlWeb();
        HtmlDocument doc = web.Load("http://example.com");

        foreach (HtmlNode link in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            HtmlAttribute att = link.Attributes["href"];
            Console.WriteLine(att.Value);
        }
    }
}
```

Ausgabe (beispielhaft für die extrahierten Links von `example.com`):
```
http://example.com/about
http://example.com/contact
http://example.com/login
```

## Tiefere Einblicke:
HTML-Parsing ist kein neues Konzept und war schon immer Teil des Webscrapings und der Webdatenverarbeitung. Früher griffen Entwickler oft zu Regular Expressions, aber das ist fehleranfällig und ineffizient für komplexes HTML.

Alternativen zum `HtmlAgilityPack` könnten Bibliotheken wie `AngleSharp` sein, die modernere, LINQ-ähnliche Abfragen bieten. Beim Implementieren eines HTML-Parsers sollte man bedenken, dass HTML oft nicht wohlgeformt ist; eine robuste Bibliothek kann fehlertoleranter beim Umgang mit solchem Code sein.

## Siehe auch:
- HtmlAgilityPack auf GitHub: [https://github.com/zzzprojects/html-agility-pack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub-Repo: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)