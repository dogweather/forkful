---
title:                "Analyse av HTML"
date:                  2024-01-20T15:30:05.240707-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Parsing HTML innebærer å tolke og analysere HTML-koden for å forstå dens struktur og innhold. Programmerere gjør dette for å hente ut eller manipulere data, trappe opp web scraping, eller bygge innholdsdrevne applikasjoner.

## How to: (Slik gjør du:)
For å parse HTML i C, kan du bruke biblioteket `libxml2`. Her er et eksempel som viser grunnleggende bruk:

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc;
    htmlNodePtr root;

    // Parse HTML fra en streng
    const char *html = "<html><body><p>Hello, Norway!</p></body></html>";
    doc = htmlReadDoc((xmlChar*)html, NULL, NULL, HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    // Få rotnoden
    root = xmlDocGetRootElement(doc);
    
    // Skriv ut rotnoden
    printf("Root element: %s\n", (char *)root->name);

    // Rydd opp
    xmlFreeDoc(doc);
    xmlCleanupParser();

    return 0;
}
```

Eksempelutdata:
```
Root element: html
```

## Deep Dive (Dypdykk)
Parsing HTML med C har ikke alltid vært greit. Før biblioteker som `libxml2`, måtte man ofte skrive sin egen parser, et arbeid som er både komplekst og feilutsatt.

Alternativer inkluderer å bruke regulære uttrykk for enkel dataekstraksjon eller koble seg til en nettlesermotor for å gjøre jobben. Men disse metodene kommer med egne problemer – regex er upålitelig for komplekse HTML og nettlesermotorer er tyngre løsninger.

Det settes pris på `libxml2` fordi det tilbyr en rimelig balanse av ytelse og fleksibilitet. Det håndterer feil i HTML på en vennlig måte og tar seg av utfordringene som kommer med å tolke "ekte verden" HTML.

## See Also (Se Også)
- Offisiell `libxml2` nettside: http://xmlsoft.org/
- W3C HTML spesifikasjon: https://www.w3.org/TR/html52/
- TutorialsPoint's guide til `libxml2`: https://www.tutorialspoint.com/libxml/index.htm