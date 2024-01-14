---
title:                "C++: Parsa html"
simple_title:         "Parsa html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
När man surfar på internet, ser man ofta komplexa webbsidor med olika element som text, bilder och länkar. Men för en dator är detta bara en stor mängd av kryptiska koder som behöver "översättas" för att förstås. Det är här parsing av HTML kommer in i bilden. Genom att använda sig av parsing kan man extrahera och behandla specifika delar av en webbsida och använda den informationen för olika ändamål, som att bygga webbskrapare eller utveckla webbinnehåll.

## Hur man gör
För att kunna parsa HTML i C++ behöver man först inkludera lämpliga bibliotek som tillgängliggör funktioner för just detta ändamål. Ett sådant populärt bibliotek är "libxml2". Sedan är det viktigt att lära sig grundläggande HTML och dess struktur för att kunna välja och extrahera rätt element. Här är ett enkelt exempel på kod som parsar en webbplats och hämtar alla länkar:
```C++
#include <libxml/HTMLParser.h>
#include <libxml/XPath.h>
#include <iostream>

int main()
{
    const char* html = "<html><body><a href='https://www.google.com'>Google</a><a href='https://www.facebook.com'>Facebook</a></body></html>";
    
    // Parsar HTML
    htmlDocPtr doc = htmlParseDoc((xmlChar *)html, NULL);
    
    // Skapar uttryck för att hämta alla länkar
    xmlXPathContextPtr xpathCtx = xmlXPathNewContext(doc);
    xmlXPathObjectPtr xpathObj = xmlXPathEvalExpression((xmlChar *) "//a/@href", xpathCtx);
    
    // Loopar igenom resultatet
    xmlNodeSetPtr nodes = xpathObj->nodesetval;
    for(int i = 0; i < nodes->nodeNr ; i++)
    {
        // Hämtar länkar och skriver ut dem
        xmlChar* link = xmlNodeListGetString(nodes->nodeTab[i]->children, 1);
        std::cout << link << std::endl;
        free(link);
    }
    
    // Frigör minne
    xmlXPathFreeObject(xpathObj);
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    
    return 0;
}
```
Detta kodblock ska resultera i följande output: https://www.google.com and https://www.facebook.com

## Djupdykning
Vid parsering av HTML behöver man ha kunskap om både HTML och XML-formatet. HTML är en XML-baserad markeringsspråk, vilket betyder att det använder tags för att strukturera innehållet. En grundläggande förståelse för HTML-element och attribut är därför nödvändig för att kunna välja de rätta delarna av en webbsida. Vidare kan även användning av reguljära uttryck vara användbart för att söka igenom och filtrera resultatet från en parsed webbsida.

## Se även
- [libxml2](http://www.xmlsoft.org/)
- [XPath](https://www.w3schools.com/xml/xpath_intro.asp)
- [Reguljära uttryck](https://www.regular-expressions.info/)