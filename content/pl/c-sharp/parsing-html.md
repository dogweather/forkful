---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:06.728712-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
"**Parsing HTML** – to proces przekształcania kodu HTML w strukturę, którą można łatwiej przetworzyć w innym języku programowania, np. C#. Robimy to, aby odnaleźć, przeczytać lub zmodyfikować konkretne elementy strony, takie jak tekst, linki czy inne dane, bez konieczności ręcznej obróbki surowego kodu.

## Jak to zrobić?
Do parsowania HTML w C# wykorzystamy bibliotekę **HtmlAgilityPack**. Jest ona dość lekka, a zarazem potężna i prosta w użyciu.

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        // Ładowanie dokumentu HTML z URL
        var url = "http://przykladowa-strona.pl";
        var web = new HtmlWeb();
        var doc = web.Load(url);

        // Wyszukanie elementu po tagu
        var nodes = doc.DocumentNode.SelectNodes("//p");
        
        foreach (var node in nodes)
        {
            Console.WriteLine(node.InnerText);
        }

        // Wyszukiwanie elementu po klasie
        var classNode = doc.DocumentNode.SelectNodes("//div[@class='klasa-przykladowa']");
        
        foreach (var node in classNode)
        {
            Console.WriteLine(node.InnerHtml);
        }
    }
}
```

Powinniśmy zobaczyć wydrukowany tekst z paragrafów (p), oraz zawartość diva o klasie „klasa-przykladowa”.

## Deep Dive
**HtmlAgilityPack** pojawił się, gdy deweloperzy zaczęli potrzebować sposobów na manipulowanie i ekstrakcję danych z HTML po stronie serwera. Alternatywnie, można użyć wyrażeń regularnych (regex), ale to z reguły trudniejsze i mniej niezawodne dla złożonych dokumentów HTML.

Inną opcją jest użycie **AngleSharp**, nowoczesnej biblioteki .NET, która oferuje jeszcze lepsze wsparcie dla nowych standardów HTML i CSS.

Implementacja parserów HTML powinna zarządzać zarówno poprawnym kodem, jak i tymi z błędami (których w prawdziwym świecie HTML jest sporo). HtmlAgilityPack dobrze radzi sobie z takimi przypadkami, działając podobnie do przeglądarek internetowych, które interpretują nawet nieco „zepsuty” HTML.

## Zobacz także
- HtmlAgilityPack na NuGet: https://www.nuget.org/packages/HtmlAgilityPack/
- Dokumentacja HtmlAgilityPack: https://html-agility-pack.net/
- Projekt AngleSharp na GitHub: https://github.com/AngleSharp/AngleSharp
- Tutorial wideo do HtmlAgilityPack: [Link do odpowiedniego tutorialu na YouTube lub innym serwisie wideo]
