---
title:                "Parsning av html"
html_title:           "Java: Parsning av html"
simple_title:         "Parsning av html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Innan vi börjar koda, låt oss förstå varför någon skulle vilja utföra parsing av HTML i Java. 

HTML är det vanligaste språket för att skapa webbsidor och det finns en mängd olika verktyg som genererar HTML-kod. Att kunna hantera, filtrera och extrahera information från HTML-kod är därför en viktig färdighet för Java-utvecklare.

## Hur man gör
Det finns flera olika metoder för att utföra parsing av HTML i Java. Här är ett enkelt exempel med hjälp av biblioteket Jsoup:

```Java 
// importera nödvändiga bibliotek
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// skapa en anslutning till webbsidan
String url = "https://www.example.com";
Document doc = Jsoup.connect(url).get();

// välj de element som du vill extrahera information från
Elements titles = doc.select("h1");

// loopa genom elementen och skriv ut titlarna
for (Element title : titles) {
  System.out.println(title.text());
}

```
Detta kodexempel hämtar innehållet från webbsidan https://www.example.com och väljer sedan alla h1-element (vanligtvis använda för titlar) från HTML-koden. De valda titlarna skrivs sedan ut till konsolen.

Output:
```
Welcome to Example!
```

## Djupdykning
Det finns olika sätt att filtrera och extrahera information från HTML-kod i Java, beroende på vilka bibliotek och verktyg du väljer att använda. En annan populär metod är att använda DOM (Document Object Model) och XPath för att identifiera och manipulera specifika HTML-element.

Det är också viktigt att förstå skillnaden mellan static och dynamic parsing. Med en static parser, som Jsoup, hämtas HTML-koden vid en viss tidpunkt och all information som behövs måste finnas tillgänglig vid detta tillfälle. Med en dynamic parser, som Selenium, simuleras en webbläsare och HTML-koden kan hämtas och manipuleras i realtid.

Det finns även andra aspekter att tänka på när man utför parsing av HTML, som att hantera felaktig eller ovalid HTML-kod och att undvika webbsidor som försöker blockera automatisk parsing.

## Se även 
- [Jsoup dokumentation](https://jsoup.org/)
- [DOM (Document Object Model) introduktion](https://developer.mozilla.org/sv/docs/Web/API/Document_Object_Model/Introduction)
- [Selenium dokumentation](https://www.selenium.dev/documentation/en/)