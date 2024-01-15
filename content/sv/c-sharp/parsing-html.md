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

## Varför
Har du någonsin behövt extrahera information från en webbsida, men kämpat med att hitta en effektiv metod? Då är HTML parsing det du behöver! Genom att använda C# kan du enkelt hämta och manipulera data från HTML-kod, vilket sparar tid och ansträngning jämfört med manuell bearbetning.

## Hur man gör
Först och främst måste du importera "HtmlAgilityPack" -paketet i ditt C# -projekt. Detta ger dig nödvändiga verktyg för att analysera HTML-kod. Sedan kan du använda "HtmlWeb" -objektet för att hämta en webbsida och sedan använda "HtmlDocument" -objektet för att få åtkomst till dess element. Nedan finns exempel på hur man hämtar all text i en paragraf på en webbsida och skriver ut den:

```C#
HtmlWeb web = new HtmlWeb(); // Skapar ett HtmlWeb-objekt
HtmlDocument doc = web.Load("https://exempelsida.com"); // Hämtar en websida och skapar ett HtmlDocument-objekt
HtmlNode paragraph = doc.DocumentNode.SelectSingleNode("//p"); // SelectSingleNode väljer ett element baserat på XPath
Console.WriteLine(paragraph.InnerText); // Skriver ut texten i det valda elementet
```

Det finns naturligtvis många fler metoder och verktyg för att manipulera och analysera HTML-kod. Om du behöver jobba med specifika element eller attribut, kan du använda metoder som "SelectNodes" och "GetAttributeValue". Utforska dokumentationen för "HtmlAgilityPack" för att lära dig mer om dess kapaciteter och olika användningsområden.

## Djupdykning
Med "HtmlAgilityPack" kan du inte bara hämta data från HTML-kod, utan också göra ändringar i koden. Detta är användbart om du till exempel behöver uppdatera en webbsida automatiskt baserat på externt hämtade uppgifter. Genom att använda egenskaper som "InnerText" och "SetAttributeValue", kan du ändra eller lägga till data i existerande element.

Det finns också möjlighet att hantera felaktig eller ogiltig HTML-kod genom att använda metoden "LoadHtml" istället för "Load". Detta tillåter dig att bearbeta kod som annars skulle orsaka problem eller komplikationer.

## Se också
- [HtmlAgilityPack dokumentation](https://html-agility-pack.net/documentation)
- [ASP.NET Core Razor Pages tutorial: Working with HTML forms in an ASP.NET Core Razor Pages project](https://docs.microsoft.com/en-us/aspnet/core/tutorials/razor-pages/validation?view=aspnetcore-5.0&tabs=visual-studio)
- [Codecademy C# tutorial](https://www.codecademy.com/learn/learn-c-sharp)