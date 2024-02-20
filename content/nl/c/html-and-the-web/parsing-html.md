---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:41.083160-07:00
description: "Het parsen van HTML in C omvat het analyseren van HTML-documenten om\
  \ gegevens, structuur of specifieke onderdelen effici\xEBnt te extraheren, vaak\
  \ als\u2026"
lastmod: 2024-02-19 22:05:10.377432
model: gpt-4-0125-preview
summary: "Het parsen van HTML in C omvat het analyseren van HTML-documenten om gegevens,\
  \ structuur of specifieke onderdelen effici\xEBnt te extraheren, vaak als\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het parsen van HTML in C omvat het analyseren van HTML-documenten om gegevens, structuur of specifieke onderdelen efficiënt te extraheren, vaak als voorloper van datamining of webscraping. Programmeurs doen dit om de automatische extractie van informatie mogelijk te maken, waardoor webinhoud programmatisch verwerkt of hergebruikt kan worden.

## Hoe:

HTML parsen kan ontmoedigend lijken door de complexiteit van HTML en de frequente afwijkingen van schone, goed gevormde structuren. Echter, het gebruik van een bibliotheek zoals `libxml2`, specifiek zijn HTML-parsingmodule, vereenvoudigt het proces. Dit voorbeeld demonstreert hoe je `libxml2` kunt gebruiken om HTML te parsen en informatie te extraheren.

Zorg eerst dat `libxml2` is geïnstalleerd in je omgeving. In veel Linux-distributies kun je het via de pakketbeheerder installeren. Bijvoorbeeld, op Ubuntu:

```bash
sudo apt-get install libxml2 libxml2-dev
```

Nu gaan we een eenvoudig C-programma schrijven dat `libxml2` gebruikt om een HTML-string te parsen en de tekst binnen een specifiek element af te drukken:

```c
#include <stdio.h>
#include <libxml/HTMLparser.h>

void parseHTML(const char *html) {
    htmlDocPtr doc = htmlReadDoc((const xmlChar *)html, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
    
    // Uitgaand van het zoeken naar inhoud binnen <p> tags
    xmlNode *root_element = xmlDocGetRootElement(doc);
    for (xmlNode *current_node = root_element; current_node; current_node = current_node->next) {
        if (current_node->type == XML_ELEMENT_NODE && strcmp((const char *)current_node->name, "p") == 0) {
            printf("Gevonden paragraaf: %s\n", xmlNodeGetContent(current_node));
        }
    }
    
    xmlFreeDoc(doc);
    xmlCleanupParser();
}

int main() {
    const char *html = "<html><body><p>Hallo, wereld!</p></body></html>";
    parseHTML(html);
    return 0;
}
```

Voorbeelduitvoer:
```
Gevonden paragraaf: Hallo, wereld!
```

Dit voorbeeld richt zich op het extraheren van tekst binnen paragraaftags, maar `libxml2` biedt robuuste ondersteuning voor het navigeren door en opvragen van verschillende delen van een HTML-document.

## Diepere Duik

Het parsen van HTML in C gaat terug tot de vroege dagen van webontwikkeling. Aanvankelijk moesten ontwikkelaars vertrouwen op aangepaste, vaak rudimentaire parsoplossingen, vanwege het gebrek aan gestandaardiseerde bibliotheken en de chaotische staat van HTML op het web. De introductie van bibliotheken zoals `libxml2` markeerde een significante vooruitgang, en bood meer gestandaardiseerde, efficiënte en veerkrachtige benaderingen voor het parsen van HTML.

Ondanks de ongeëvenaarde snelheid en controle van C, is het vermeldenswaard dat C niet altijd het beste gereedschap is voor het parsen van HTML, vooral voor taken die snelle ontwikkelingscycli vereisen of te maken hebben met uitzonderlijk slecht gevormde HTML. Talen met high-level HTML-parsingbibliotheken, zoals Python met Beautiful Soup, bieden meer abstracte, gebruiksvriendelijke interfaces ten koste van een beetje prestatie.

Desalniettemin, voor prestatie-kritieke applicaties, of wanneer men werkt in een omgeving met beperkte middelen, blijft het parsen van HTML in C een haalbare en vaak voorkeursmethode. De sleutel is het benutten van robuuste bibliotheken zoals `libxml2` om de complexiteiten van HTML aan te pakken, waardoor ontwikkelaars zich kunnen concentreren op het extraheren van de gegevens die ze nodig hebben zonder te verzanden in de details van de parsemechanica.
