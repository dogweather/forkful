---
title:                "Att tolka html"
html_title:           "C#: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Parsing av HTML är en vanlig uppgift för programmerare som innebär att tolka och extrahera data från HTML-dokument. Detta är viktigt eftersom det ger möjlighet att hämta specifik information från en webbsida för att använda den i en applikation eller för att utföra automatiska tester.

## Hur gör man:
För att utföra parsing av HTML i C# finns det flera tredjepartsbibliotek tillgängliga, till exempel HtmlAgilityPack eller AngleSharp. Dessa bibliotek tillhandahåller enkla metoder för att ladda ett HTML-dokument från en URL eller en fil och sedan navigera genom dokumentet för att hitta och extrahera önskad data.

Ett exempel på hur man kan använda HtmlAgilityPack för att hämta alla länkar från en webbsida:

```C#
HtmlWeb web = new HtmlWeb();
HtmlDocument dokument = web.Load("https://www.example.com");
HtmlNodeCollection länkar = dokument.DocumentNode.SelectNodes("//a");
foreach (HtmlNode länk in länkar)
{
    Console.WriteLine(länk.Attributes["href"].Value);
}
```

Detta kodexempel använder HtmlWeb-klassen för att hämta HTML-dokumentet från Url:en och sedan använder SelectNodes-metoden för att hitta alla länkelement på sidan. Genom att loopa igenom denna samling av element kan vi sedan skriva ut länkarnas adresser till konsolen.

## Djupdykning:
Parsing av HTML har funnits sedan de tidiga dagarna av webbutveckling och det finns flera olika metoder och verktyg för att hantera det. En alternativ metod är att använda reguljära uttryck för att matcha och extrahera data från HTML, men detta kan bli komplicerat och svårt att underhålla. Tredjepartsbibliotek som HtmlAgilityPack erbjuder en mer strukturerad och pålitlig lösning.

Genom att förstå HTML-strukturen och kunna använda HTML-klasser och ID:er kan du styra vilken data som ska extraheras och hur den ska behandlas. Denna kunskap kan vara användbar både för webbutveckling och datainsamling.

## Se även:
- [HtmlAgilityPack](https://html-agility-pack.net/)
- [AngleSharp](https://anglesharp.github.io/)
- [Regular Expressions for HTML Parsing?](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)