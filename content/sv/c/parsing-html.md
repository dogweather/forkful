---
title:                "C: Analysera html"
simple_title:         "Analysera html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-html.md"
---

{{< edit_this_page >}}

## Varför

HTML-analys är en viktig del av webbutveckling eftersom det tillåter oss att extrahera viktig information från webbsidor. Det kan vara användbart för automatisering av uppgifter som att hämta nyhetsrubriker, jämföra priser eller för analys av webbanvändning.

## Så här gör du

För att analysera HTML i C kan vi använda biblioteket libxml2. Detta bibliotek ger en enkel och effektiv metod för att extrahera data från HTML-dokument.

Först måste vi inkludera libxml2-biblioteket och definiera en pekare till ett HTML-dokument.

```C
#include <libxml/HTMLparser.h>

xmlDocPtr doc; // pekare till dokumentet
```

Sedan använder vi funktionen `htmlParseFile` för att öppna och analysera HTML-filen.

```C
doc = htmlParseFile("html_document.html", "utf-8");
```
För att få tag på en viss tagg, till exempel en `<h1>`-tagg, kan vi använda funktionen `xmlGetElementsByTagName` och sedan loopa igenom alla h1-taggar och skriva ut deras innehåll.

```C
xmlNodeList * h1Tags = xmlGetElementsByTagName(doc, "h1");

int i;
for (i = 0; i < h1Tags->length; i++) {
    xmlNode * node = h1Tags->nodeTab[i];
    xmlChar * content = xmlNodeGetContent(node);
    printf("Titel: %s\n", content);
    xmlFree(content);
}
```

Det resulterande utskriftsfönstret skulle se ut som följande:

```console
Titel: Min första webbsida
Titel: Det här är en annan rubrik
```

## Djupdykning

Libxml2 har ett brett utbud av olika funktioner för att hantera HTML-dokument, inklusive möjligheten att hämta attribut, navigera i dokumentet och hantera fel. Det är också möjligt att använda XPath för att söka efter specifika element i dokumentet. Libxml2 har en omfattande dokumentation som presenterar alla dessa funktioner i detalj.

En annan viktig aspekt av HTML-analys är att se till att dokumentet är giltigt och följer HTML-specifikationerna. Libxml2 har stöd för att validera dokumentet mot DTD eller schemafiler.

## Se även

- [Libxml2 dokumentation](http://www.xmlsoft.org/html/libxml-html.html)
- [HTML-specifikationer](https://www.w3.org/TR/html52/)
- [HTML-analys med libxml2 tutorial](https://www.xml.com/pub/a/1999/09/xmlontheweb/part3.html) (på engelska)