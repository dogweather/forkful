---
title:                "C#: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
HTML är ett standardformat för att skapa webbsidor, men det kan vara svårt att extrahera information från dessa sidor om man inte har rätt verktyg. Genom att lära sig hur man parsar HTML kan man lättare få ut den information man behöver för att automatisera uppgifter eller utföra dataanalyser.

## Hur man parsar HTML i C#
För att parsar HTML i C# finns det flera olika bibliotek att välja mellan, men i den här bloggposten kommer vi att använda HtmlAgilityPack. Här är ett exempel på hur man kan använda detta bibliotek för att plocka ut information från en HTML-sida:

```C#
var web = new HtmlWeb();
var doc = web.Load("https://www.example.com");

// Hämta alla länkar från hemsidan
var links = doc.DocumentNode.SelectNodes("//a[@href]");
foreach (var link in links)
{
    Console.WriteLine(link.Attributes["href"].Value);
}
```

Detta kodexempel hämtar alla länkar från en given URL och skriver ut dem i konsolen. Genom att använda olika XPath-uttryck kan man välja vilken typ av information man vill hämta från en hemsida.

## Djupdykning
Det finns många olika faktorer att tänka på när man parsar HTML, till exempel att hantera felaktigt formaterad HTML eller att kunna hantera dynamiskt genererade sidor. Det kan också vara användbart att använda Regular Expressions för att filtrera och bearbeta den hämtade datan.

En annan viktig faktor att tänka på är att inte överbelasta hemsidan man hämtar från. Genom att sätta en lämplig tidsfördröjning mellan varje HTTP-begäran kan man minimera risken för att bli blockerad eller märkt som en spam-bot.

## Se även
- HtmlAgilityPack Dokumentation: https://html-agility-pack.net/
- XPath Tutorial: https://www.w3schools.com/xml/xpath_intro.asp
- Regular Expressions in C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference