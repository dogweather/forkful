---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML (Parsing HTML) innebär att bearbeta HTML-dokument till en struktur som koden kan förstå och interagera med. Vi utför detta när vi vill läsa, manipulera, och extrahera information från webbsidor.

## Så här gör du:
Ett enkelt sätt att analysera HTML i C# är att använda HtmlAgilityPack-biblioteket. Här är ett exempel:

```C#
using HtmlAgilityPack;
    
static void Main(string[] args)
{
    var webGet = new HtmlWeb();
    var document = webGet.Load("https://www.yourwebsite.com");
    
    // Hitta alla 'h1' taggar på webbsidan
    var nodes = document.DocumentNode.SelectNodes("//h1");
    
    foreach (var node in nodes)
    {
        Console.WriteLine(node.InnerHtml);
    }
    
    Console.ReadLine();
}
```

I det här exemplet laddar vi HTML från en webbsida, letar efter alla 'h1'-element och skriver ut deras innehåll.

## Djupdykning
Historiskt sett har programvaror för HTML-parsing funnits sedan början på World Wide Web, men C# -språket och .NET Core har effektiviserat och förenklat processen.

Alternativa bibliotek inkluderar AngleSharp, vilket har stöd för många webbstandarder inklusive HTML5 och CSS3.

En viktig aspekt av att analysera HTML är att förstå hur DOM (Document Object Model) fungerar. Detta är strukturen som HTML tolkas till och det som programmet sedan interagerar med.

## Se även
1. HtmlAgilityPack Tutorial: [link](https://html-agility-pack.net/)
2. Mer om DOM: [link](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
3. AngleSharp: [link](https://anglesharp.github.io/)