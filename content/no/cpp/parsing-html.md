---
title:                "Analyse av HTML"
date:                  2024-01-20T15:30:47.274465-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML betyr å tolke og omforme HTML-koden til noe programmet kan forstå og jobbe med. Programmerere gjør dette for å hente ut, manipulere eller sjekke innhold på nettsider.

## Slik gjør du:
For å parse HTML i C++ kan vi bruke biblioteket `libxml2` som eksempel. Dette er raskt og effektivt.

```C++
#include <libxml/HTMLparser.h>
#include <iostream>

int main() {
    const char* htmlContent = "<html><body><p>Hei, Norge!</p></body></html>";
    htmlDocPtr doc = htmlReadDoc((xmlChar*)htmlContent, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    xmlNode *root_element = xmlDocGetRootElement(doc);

    // Enkel gjennomgang av elementene
    for(xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if(currentNode->type == XML_ELEMENT_NODE) {
            std::cout << "Element: " << currentNode->name << std::endl;
        }
    }

    // Rydde opp
    xmlFreeDoc(doc);
    xmlCleanupParser();

    return 0;
}
```

Forventet utskrift vil være elementnavnet på HTML-strukturen:

```
Element: html
Element: body
Element: p
```

## Dykk ned i det:
Parsing av HTML har vært viktig siden webbens barndom. Biblioteker som `libxml2` eller `Gumbo` (fra Google) er moderne alternativer. Historisk sett var HTML-parsing vanskelig på grunn av 'tag soup', eller dårlig strukturert HTML. Moderne verktøy håndterer dette bedre.

1. **Historisk kontekst:**
   HTML ble opprettet på begynnelsen av 90-tallet. Tidlige parsers var strenge, men måtte utvikle toleranse for dårlig HTML ettersom webben vokste.

2. **Alternativer:**
   I C++-samfunnet, utenom `libxml2`, finnes det mange biblioteker som `Gumbo`, `MyHTML`, eller `htmlcxx`. Ulike libraries har forskjellig fokus på hastighet, brukervennlighet eller kompatibilitet.

3. **Implementasjonsdetaljer:**
   En HTML-parser bør håndtere ugyldig markup, være rask og minneeffektiv, og støtte DOM-manipulasjoner. `libxml2` er et eksempel på et bibliotek som oppfyller disse kravene og er vidt brukt både i åpen kildekode og kommersielt.

## Se også:
- `libxml2` offisielle nettside: http://xmlsoft.org/
- Gumbo-parseren: https://github.com/google/gumbo-parser
- W3C Markup Validation Service: https://validator.w3.org/
- MyHTML GitHub repo: https://github.com/lexborisov/myhtml
