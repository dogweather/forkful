---
title:                "HTML aufspalten."
html_title:           "C#: HTML aufspalten."
simple_title:         "HTML aufspalten."
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# Warum

Das Parsen von HTML ist ein unverzichtbarer Prozess für die Webentwicklung. Es ermöglicht uns, Daten aus Webseiten zu extrahieren und für verschiedene Zwecke zu verwenden, wie z.B. für Web Scraping oder Datenanalyse.

# Wie geht es

Um HTML in C# zu parsen, können wir die HtmlAgilityPack-Bibliothek verwenden. Diese Bibliothek bietet zahlreiche Funktionen, um HTML zu durchsuchen und Daten zu extrahieren.

```C#
// Installiere das HtmlAgilityPack über NuGet
Install-Package HtmlAgilityPack

// Importiere das HtmlAgilityPack Namespace
using HtmlAgilityPack;

// Erstelle ein HtmlDocument Objekt
HtmlDocument doc = new HtmlDocument();

// Lade die HTML-Datei
doc.Load("sample.html");

// Wähle ein bestimmtes Element mit der Methode SelectSingleNode aus (z.B. ein <h1> tag)
HtmlNode node = doc.DocumentNode.SelectSingleNode("//h1");

// Gebe den Inhalt des Tags aus
Console.WriteLine(node.InnerHtml); // Output: "Willkommen auf meiner Webseite"
```
## Deep Dive

Es gibt verschiedene Möglichkeiten, um HTML in C# zu parsen. Eine andere Möglichkeit ist die Verwendung der .NET-eigenen WebClient-Klasse, um den Quellcode einer Webseite herunterzuladen, und dann Regex-Ausdrücke zu verwenden, um die gewünschten Daten zu extrahieren. Diese Methode kann jedoch komplexer und weniger stabil sein.

Außerdem gibt es beim Parsen von HTML häufig auftretende Probleme wie z.B. fehlende oder falsch formatierte Tags. In solchen Fällen kann das HtmlAgilityPack mit seinem automatischen Tag- und Strukturkorrekturmechanismus helfen.

# Siehe auch

- [Offizielle HtmlAgilityPack Dokumentation](https://html-agility-pack.net/documentation)
- [Tutorial: Scraping HTML-Daten mit C# und HtmlAgilityPack](https://www.scrapingbee.com/blog/web-scraping-csharp/)