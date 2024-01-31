---
title:                "Tolka HTML"
date:                  2024-01-20T15:30:33.793728-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML innebär att analysera HTML-koden för att förstå och manipulera dess struktur och innehåll. Programmerare gör detta för att kunna interagera dynamiskt med webbsidor, extrahera data, testa webbapplikationers gränssnitt eller skapa webbskrapor.

## Steg för steg:
För enkelhetens skull, låt oss använda biblioteket "Gumbo-parser" som är en C-bibliotek för att parsa HTML5. Vi wrappar den i en C++ kontext för att hålla det modernt.

```C++
#include <iostream>
#include <string>
#include <gumbo.h>

void search_for_text(GumboNode* node) {
    if (node->type != GUMBO_NODE_TEXT) return;
    std::cout << node->v.text.text << std::endl;
}

void parse_html(const std::string& html) {
    GumboOutput* output = gumbo_parse(html.c_str());
    search_for_text(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}

int main() {
    std::string html = "<!DOCTYPE html><html><body><p>Hej världen!</p></body></html>";
    parse_html(html);
    return 0;
}
```
Kör du det här programmet skriver det ut `Hej världen!`.

## Djupdykning:
HTML-parsingens historia är lika gammal som webben. Från regex-hack till sofistikerade DOM-parsers har tekniken utvecklats mycket. En av de första DOM-baserade parsers var "libxml2" för XML, som senare anpassades för HTML.

Alternativ till Gumbo-parser kan vara C++-biblioteket "htmlcxx" eller användning av "libxml2" direkt. Förutom dedikerade bibliotek, finns det också hela webbläsarmotorer som WebKit eller Gecko som kan parsa och rendera HTML, men de är ofta överkvalificerade för enkel parsing.

När man implementerar HTML-parsing bör man vara medveten om att HTML är ett komplex språk med många implicita regler och undantag. Parser som Gumbo tar hand om dessa genom att tolka HTML liknande hur moderna webbläsare gör det. Viktigt är också att fundera på prestanda och minneshantering; webbsidor kan bli stora och parsers behöver hantera detta effektivt.

## Se även:
- Gumbo-parser GitHub-sida: https://github.com/google/gumbo-parser
- C++ parsing library htmlcxx: http://htmlcxx.sourceforge.net/
- XML och HTML parsing med libxml2: http://www.xmlsoft.org/html/libxml-HTMLintro.html
