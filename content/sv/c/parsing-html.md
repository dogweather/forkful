---
title:                "Tolka HTML"
date:                  2024-01-20T15:30:35.875822-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av HTML innebär att man tolkar och bearbetar HTML-kod för att förstå dess struktur och innehåll. Programmerare gör detta för att extrahera data, manipulera innehåll och integrera webbsidor med applikationer.

## Hur gör man?:
För att parsa HTML i C kan vi använda `libxml2`, ett bibliotek skrivet för C som stödjer diverse XML-baserade teknologier, inklusive XHTML som är nära besläktad med HTML.

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    const char *htmlContent = "<html><body><p>Hej, Sverige!</p></body></html>";
    htmlDocPtr doc = htmlReadMemory(htmlContent, strlen(htmlContent), NULL, NULL, 0);

    xmlNode *root_element = xmlDocGetRootElement(doc);
    printf("Root element is: %s\n", root_element->name);
    
    // Rena upp och avsluta
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 0;
}
```

Sample output:
```
Root element is: html
```

## Djupdykning:
Parsing av HTML är en komplex process som blivit mer strukturerad med åren. Tidigare skedde ofta parsing med reguljära uttryck, vilket inte är rekommenderat då HTML inte är ett reguljärt språk. `libxml2` används ofta eftersom det är robust, har stöd för flera språk och standarder, och tar hand om de finesser som HTML5 för med sig. Andra alternativ inkluderar `Gumbo` och `MyHTML`. En bra parser hanterar inte bara korrekt formaterad HTML utan också dåligt formatterad källkod, vilket är vanligt på webben.

## Se också:
- `libxml2` dokumentation: http://xmlsoft.org/html/libxml-HTMLparser.html
- W3C Markup Validation Service: https://validator.w3.org/
- HTML parsing i Python med Beautiful Soup: https://www.crummy.com/software/BeautifulSoup/
