---
title:                "Analysering av HTML"
html_title:           "C++: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har jobbet med å hente data fra et nettsted, har du sannsynligvis støtt på HTML. Dette språket brukes til å strukturere og formatere innholdet på nettsider. Ved å lære å parse HTML med C++, kan du hente ut spesifikke data fra nettsider og bruke det til å bygge applikasjoner, nettsteder eller automatisere oppgaver.

## Hvordan gjøre det
For å parse HTML med C++, trenger du et bibliotek kalt `libxml2` som kan lastes ned fra nettet. Når du har lastet ned og installert biblioteket, kan du bruke følgende kode for å hente og parse HTML fra en nettside:

```C++
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main()
{
    // Hent HTML-kildekode fra en nettside
    const char* htmlKilde = "<html><body><h1>Velkommen</h1><p>Dette er en test</p></body></html>";

    // Opprett en HTML-parserkontekst
    htmlParserCtxtPtr parser = htmlCreateMemoryParserCtxt(htmlKilde, strlen(htmlKilde));

    // Analyser HTML-kildekoden
    htmlDocPtr doc = htmlCtxtReadDoc(parser, NULL, "UTF-8", HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    // Hent dokumentets rotelement (html-taggen)
    xmlNodePtr rotelement = xmlDocGetRootElement(doc);

    // Hent og skriv ut innholdet i alle <p>-tagger
    for (xmlNodePtr node = rotelement->children; node != NULL; node = node->next) 
    {
        if (node->type == XML_ELEMENT_NODE && xmlStrcmp(node->name, (const xmlChar *) "p") == 0) 
        {
            xmlChar *innhold = xmlNodeGetContent(node);
            printf("%s\n", innhold);
            xmlFree(innhold);
        }
    }
    
    // Frigjør minne
    htmlFreeDoc(doc);
    htmlFreeParserCtxt(parser);
    
    return 0;
}
```
Dette enkle eksempelet viser hvordan du kan hente ut og skrive ut innholdet i alle `<p>`-tagger fra et HTML-dokument. Du kan bygge videre på dette eksempelet for å hente ut andre typer data eller for å bygge mer avanserte applikasjoner.

## Dypdykk
Parsing av HTML med C++ kan være en kompleks oppgave, spesielt hvis du ønsker å håndtere flere forskjellige typer HTML-dokumenter og tags. Du kan bruke `libxml2` til å håndtere en rekke forskjellige typer HTML, inkludert HTML5, XHTML og XML. Biblioteket har også mange funksjoner som kan hjelpe deg med å håndtere feil i HTML-dokumenter og konvertere data til forskjellige formater.

Et viktig konsept å forstå når du jobber med parsing av HTML er DOM (Document Object Model). Dette er en representering av HTML-dokumentet som et tre av objekter, som gjør det enkelt å navigere gjennom dokumentet og hente ut ønsket data. `libxml2` bruker også DOM-prinsipper i sin parsing.

Det kan også være lurt å sjekke ut følgende ressurser for å lære mer om parsing av HTML med C++:

- [libxml2 hjemmeside](http://www.xmlsoft.org/html/index.html)
- [libxml2 dokumentasjon](http://www.xmlsoft.org/html/libxml-HTMLparser.html)
- [W3Schools - HTML tutorial](https://www.w3schools.com/html/default.asp)
- [Stack Overflow - "parsing html with c++"](https://stackoverflow.com/questions/5328911/parsing-html-with-c)

## Se også
- [Parsing JSON i C++](https://github.com/EtienneDepaulis/cpp-json-parsing-article)