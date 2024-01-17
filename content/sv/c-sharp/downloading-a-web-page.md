---
title:                "Att ladda ner en webbsida"
html_title:           "C#: Att ladda ner en webbsida"
simple_title:         "Att ladda ner en webbsida"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta all information som visas på en viss webbadress och visa den på din egen dator. Programmare använder den här tekniken för att hämta data från webben, bearbeta den och använda den i sina egna program.

## Så här:
```C#
using System;
using System.Net;

namespace WebbHämtning
{
    class Program
    {
        static void Main(string[] args)
        {
            // Skapa en ny WebClient
            WebClient client = new WebClient();

            // Ladda ner en webbsida och spara den som en sträng
            string htmlString = client.DownloadString("https://www.example.com");

            // Skriv ut den hämtade webbsidan till konsolen
            Console.WriteLine(htmlString);
        }
    }
}
```

Output:
```html
<!doctype html>
<html>
<head>
    <title>Exempel</title>
</head>
<body>
    <h1>Välkommen till vår webbsida!</h1>
    <p>Här kan du lära dig mer om webbhämtning med C#!</p>
</body>
</html>
```

## Deep Dive:
Webbhämtning har funnits sedan början av webbens historia och har varit en viktig teknik för att hämta och bearbeta data från webben. Innan C#-kodet ovan använde programmerare ofta språk som HTML och JavaScript för att göra liknande saker. Det finns också andra bibliotek, som Selenium, som kan användas för att ladda ner webbsidor i C#.

## Se även:
- [Lär dig mer om webbhämtning i C#](https://www.w3schools.com/cs/cs_webpages.asp)
- [Hitta andra tillgängliga bibliotek för webbhämtning i C#](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)#Frameworks_and_tools)
- [Lär dig mer om hur webben fungerar](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web)