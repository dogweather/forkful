---
title:                "Att arbeta med XML"
date:                  2024-01-26T04:28:10.079430-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML i C innebär att tolka, skapa och manipulera XML-filer - i grund och botten strukturerad datalagring. Programmerare gör detta för att interagera med data i ett portabelt och läsbart format, ofta använd för konfiguration, datautbyte, och mer.

## Hur man gör:
Nedan är en kodsnutt som använder `libxml2`-biblioteket för att tolka en XML-fil och ta fram rotelementet.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Tolka XML-filen
    doc = xmlReadFile("example.xml", NULL, 0);

    // Hämta rotelementet
    root_element = xmlDocGetRootElement(doc);

    printf("Rotelement: %s\n", root_element->name);

    // Frigör dokumentet
    xmlFreeDoc(doc);

    // Städa upp tolken
    xmlCleanupParser();

    return 0;
}
```

Exempelutdata för en XML med root `<data>` kan vara:
```
Rotelement: data
```

## Fördjupning
XML, eller Extensible Markup Language, härstammar från slutet av 90-talet och erbjuder ett sätt att beskriva och strukturera data. I C är `libxml2` att föredra. Det är robust, även om det inte är det enklaste för XML-nybörjare. Alternativ inkluderar `tinyxml2`, som är lättare och mer nybörjarvänligt. När det gäller implementering, så har C inget inbyggt XML-stöd, så bibliotek fyller ut gapet. De varierar i storlek, hastighet, komplexitet och portabilitet. De flesta erbjuder DOM- och SAX-parsingmetoder: DOM laddar hela saken i minnet, bra för små dokument; SAX är händelsedriven, hanterar element på flyga, bättre för stora filer. Båda har sina användningsområden och avvägningar.

## Se även
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 på GitHub](https://github.com/leethomason/tinyxml2)
- [XML-tutorial på w3schools](https://www.w3schools.com/xml/)
- [XML-specifikation av W3C](https://www.w3.org/XML/)
