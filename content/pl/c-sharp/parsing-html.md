---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie HTML to proces analizy struktury dokumentu HTML w celu zrozumienia jego zawartości. Programiści robią to, aby łatwo manipulować strukturą dokumentu, ekstrahować dane i dynamicznie renderować stronę internetową.

## Jak to zrobić:

Aby przeprowadzić parsowanie HTML w C#, najpierw musisz zainstalować pakiet `HtmlAgilityPack` przy pomocy narzędzia NuGet. Poniżej znajduje się przykładowy fragment kodu.

```C#
using HtmlAgilityPack;

public void ParseHtml(string html)
{
    var doc = new HtmlDocument();
    doc.LoadHtml(html);

    var node = doc.DocumentNode.SelectSingleNode("//head/title");

    Console.WriteLine("Title: {0}", node.InnerHtml);
}
```

Wyjście: 
```
Title: Tytuł strony
```

Kod wczytuje dokument HTML, a następnie wybiera pierwszy element title znajdujący się wewnątrz tagu head.

## Dogłębne spojrzenie:

Parsowanie HTML ma długą historię, począwszy od czasów, gdy strony internetowe były zdecydowanie prostsze. Dzisiaj, z pojawieniem się nowszych technologii frontendowych, parsowanie HTML jest wykorzystywane do zrozumienia i manipulacji skomplikowanymi strukturami HTML zamiast prostego wyświetlania zawartości.

Alternatywą dla `HtmlAgilityPack` jest `CsQuery`, który oferuje podobne funkcje z interfejsem zapożyczonym od popularnej biblioteki JavaScript JQuery.

Ponieważ HTML jest językiem znaczników, parser HTML, tak jak nasz kod z przykładu, pracuje, identyfikując i interpretując te znaczniki, a następnie mapując je na struktury danych, które mogą być dalej manipulowane w kodzie.

## Zobacz także:

* Dokumentacja HtmlAgilityPack: https://html-agility-pack.net/
* Dokumentacja CsQuery: https://github.com/jamietre/CsQuery
* HTML DOM: https://www.w3schools.com/whatis/whatis_htmldom.asp