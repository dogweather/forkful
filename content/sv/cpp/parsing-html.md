---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att analysera (parse) HTML innebär att uppfatta och tolka HTML-kod för att förstå dess struktur och innehåll. Programmerare gör det för att extrahera specifika delar av information från webbsidor, till exempel text, länkar eller bilder.

## Så gör du:

Här är ett facade exempel på hur man analyserar HTML med hjälp av ett C++ bibliotek kallat Gumbo:

```C++
#include <iostream>
#include <gumbo.h>

int main() {
    GumboOutput* output = gumbo_parse("<h1>Hello, World!</h1>");
    GumboNode* h1 = output->root->v.element.children.data[0];
    if (h1->type == GUMBO_NODE_ELEMENT && h1->v.element.tag == GUMBO_TAG_H1) {
        GumboNode* text = h1->v.element.children.data[0];
        std::cout << text->v.text.text << std::endl;
    }
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

När du kör detta program kommer det att skriva ut `Hello, World!`

## Djupgående information

Historiskt sett har parsing av HTML varit utmanande på grund av dess flexibla syntax. Det var före introduktionen av bibliotek som Gumbo, som följer HTML5-specifikationen för parsing.

Alternativ inkluderar andra bibliotek som `libxml2` och `Beautiful Soup`, men dessa kan vara överflödiga om du bara behöver grundläggande parsing-funktioner.

När det gäller implementeringsdetaljer använder Gumbo en teknik kallad tolkning (parsing) där HTML-koden bryts ned och konstrueras till ett syntaxträd (DOM), vilket gör det enkelt att navigera och extrahera information.

## Se även

1. [Gumbo HTML-parser Github](https://github.com/google/gumbo-parser)
2. [libxml2](http://xmlsoft.org/)
3. [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
4. [Kom ihåg syntax av C++](https://www.learnpython.org/) (website but available in Swedish)
5. [Skapa en webbskrapa](https://www.codeproject.com/Articles/1041114/Webscraper-in-Cplusplus-using-Gumbo)