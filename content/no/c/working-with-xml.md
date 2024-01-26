---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:28:02.888492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML i C innebærer parsing, oppretting og manipulering av XML-filer - i bunn og grunn strukturert datalagring. Programmører gjør dette for å samhandle med data i et bærbart og menneskelesbart format, ofte brukt for konfigurasjon, datautveksling og mer.

## Hvordan:
Nedenfor er et utdrag som bruker `libxml2`-biblioteket for parsing av en XML-fil og henting av rotelementet.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Parse XML-filen
    doc = xmlReadFile("example.xml", NULL, 0);

    // Hent rotelementet
    root_element = xmlDocGetRootElement(doc);

    printf("Rot Element: %s\n", root_element->name);

    // Frigjør dokumentet
    xmlFreeDoc(doc);

    // Rens opp parseren
    xmlCleanupParser();

    return 0;
}
```

Eksempelutdata for en XML med rot `<data>` kan være:
```
Rot Element: data
```

## Dypdykk
XML, eller Extensible Markup Language, stammer fra slutten av 90-tallet og tilbyr en måte å beskrive og strukturere data på. I C er `libxml2` det foretrukne biblioteket. Det er robust, skjønt ikke det enkleste for XML-nybegynnere. Alternativer inkluderer `tinyxml2`, som er lettere og mer nybegynnervennlig. Når det gjelder implementasjon, har ikke C innebygd støtte for XML, så biblioteker fyller dette gapet. De varierer i størrelse, hastighet, kompleksitet og bærbarhet. De fleste tilbyr DOM og SAX parsing metoder: DOM laster hele greia inn i minnet, bra for små dokumenter; SAX er hendelsesdrevet, håndterer elementer på flue, bedre for store filer. Begge har sine bruksområder og avveininger.

## Se Også
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 på GitHub](https://github.com/leethomason/tinyxml2)
- [XML-tutorial på w3schools](https://www.w3schools.com/xml/)
- [XML-spesifikasjon av W3C](https://www.w3.org/XML/)