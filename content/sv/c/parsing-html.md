---
title:                "Parsera html"
html_title:           "C: Parsera html"
simple_title:         "Parsera html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

Du undrar kanske varför det är viktigt att kunna parsa HTML i C, den senaste versionen av det populära programmeringsspråket. Svaret är enkelt: HTML är språket i vilket webbsidor skrivs, och genom att kunna parsa det kan du extrahera och manipulera data från internet på ett smidigt sätt. Detta kan vara användbart för allt från webbsskrapning till automatiserad databehandling.

## Så här gör du

För att kunna parsa HTML i C behöver du först och främst lära dig hur man bearbetar strängar, eftersom HTML är skrivet som en sträng. Använd funktionen `strstr()` för att hitta specifika taggar och `strtok()` för att dela upp strängen i mindre delar. Här är ett exempel på hur du skulle kunna extrahera all text mellan ett `<h1>`-tagg från en HTML-sida:

```C
char html[] = "<h1>Hej världen!</h1>";
char* start = strstr(html, "<h1>") + 4; // Hittar första förekomsten av "<h1>" och flyttar startpekaren 4 steg framåt 
char* end = strstr(start, "</h1>"); // Hittar första förekomsten av "</h1>"
*end = '\0'; // Sätter en nolltecken vid slutet av strängen
printf("%s", start); // Skriver ut "Hej världen!"
```

## Djupdykning

Att parsa HTML handlar inte bara om att hitta och manipulera enskilda taggar, utan också om att kunna navigera genom en hel HTML-struktur. Det finns flera tillgängliga tredjepartsbibliotek som kan hjälpa dig med detta, som till exempel libxml2 eller BeautifulSoup. Dessa bibliotek har redan implementerat komplexa funktioner för att söka, extrahera och bearbeta HTML-data. Det kan vara värt att utforska dessa bibliotek om du behöver en mer avancerad lösning för att parsar HTML i C.

## Se även

- [libxml2](http://www.xmlsoft.org/)
- [BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/)