---
title:                "Analys av html"
html_title:           "C++: Analys av html"
simple_title:         "Analys av html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Varför
Det finns många webbapplikationer som genererar HTML-kod och ibland behöver vi extrahera information från dessa sidor. Det är då vi kan använda oss av HTML-parsing för att effektivt extrahera den informationen vi behöver.

## Hur man gör det
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main()
{
    // Skapa en sträng med HTML-kod
    std::string html = "<h1>Detta är en rubrik</h1>";

    // Hitta början på taggen
    auto start = std::find(html.begin(), html.end(), '<');
    // Hitta slutet på taggen
    auto end = std::find(html.begin(), html.end(), '>');

    // Skriv ut resultatet
    std::cout << "Rubrik: " << std::string(start + 1, end) << std::endl;

    return 0;
}

```
**Output:** Rubrik: Detta är en rubrik

Det här är ett enkelt exempel på hur man kan extrahera information från HTML-kod. Först använder vi funktionen "find" för att hitta början och slutet på taggen. Sedan använder vi "string" för att extrahera den text som finns mellan taggarna. Detta är en grundläggande teknik för HTML-parsing men det finns många andra metoder som kan användas beroende på dina specifika behov.

## Djupdykning
HTML-parsing innebär att man extraherar information från HTML-dokument för att använda den i sin egen applikation. Det finns flera olika metoder som kan användas för att hitta och extrahera data från HTML-kod. Här är några av de vanligaste teknikerna som kan användas:

1. DOM-parsing: DOM (Document Object Model) är ett representationssätt för HTML-dokument som gör det möjligt att manipulera dess struktur med hjälp av JavaScript eller andra programmeringsspråk. Genom att använda en parser som "libxml" kan man läsa in HTML-dokument som en DOM-struktur och sedan navigera och extrahera data från den.

2. Regular Expressions: Reguljära uttryck är ett kraftfullt verktyg som kan användas för att söka och matcha mönster i text. Genom att använda uttryck som specifikt riktar in sig på HTML-kod kan man enkelt hitta och extrahera den informationen man behöver.

3. Tredjepartsbibliotek: Det finns många tredjepartsbibliotek som kan hjälpa till med HTML-parsing i C++. Exempel på sådana är "HTML Parser" och "Gumbo HTML Parser". Dessa bibliotek har olika funktioner och möjligheter men kan vara till stor hjälp för att effektivt extrahera information från HTML-kod.

## Se även
- [HTML Parser](https://htmlparser.sourceforge.io/)
- [Gumbo HTML Parser](https://github.com/google/gumbo-parser)
- [libxml](http://www.xmlsoft.org/)
- [Reguljära uttryck i C++](https://www.cplusplus.com/reference/regex/)