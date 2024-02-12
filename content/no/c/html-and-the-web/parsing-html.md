---
title:                "Analysering av HTML"
aliases:
- /no/c/parsing-html/
date:                  2024-02-03T17:59:54.287380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML i C innebærer analyse av HTML-dokumenter for effektivt å trekke ut data, struktur eller spesifikke deler, ofte som et forstadium til datamining eller webskraping. Programmerere gjør dette for å automatisere informasjonsekstraksjon, noe som muliggjør programmatisk behandling eller gjenbruk av webinnhold.

## Hvordan:

Å parse HTML kan virke avskrekkende på grunn av HTMLs kompleksitet og dens hyppige avvik fra rene, velformede strukturer. Men ved å bruke et bibliotek som `libxml2`, spesifikt dets HTML-parsemodul, forenkles prosessen. Dette eksempelet demonstrerer hvordan man bruker `libxml2` for å parse HTML og trekke ut informasjon.

Først, sørg for at `libxml2` er installert i ditt miljø. I mange Linux-distribusjoner kan du installere det via pakkehåndtereren. For eksempel, på Ubuntu:

```bash
sudo apt-get install libxml2 libxml2-dev
```

Nå, la oss skrive et enkelt C-program som bruker `libxml2` for å parse en HTML-streng og skrive ut teksten inne i et spesifikt element:

```c
#include <stdio.h>
#include <libxml/HTMLparser.h>

void parseHTML(const char *html) {
    htmlDocPtr doc = htmlReadDoc((const xmlChar *)html, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
    
    // Anta at vi leter etter innholdet inne i <p>-tagger
    xmlNode *root_element = xmlDocGetRootElement(doc);
    for (xmlNode *current_node = root_element; current_node; current_node = current_node->next) {
        if (current_node->type == XML_ELEMENT_NODE && strcmp((const char *)current_node->name, "p") == 0) {
            printf("Fant avsnitt: %s\n", xmlNodeGetContent(current_node));
        }
    }
    
    xmlFreeDoc(doc);
    xmlCleanupParser();
}

int main() {
    const char *html = "<html><body><p>Hei, verden!</p></body></html>";
    parseHTML(html);
    return 0;
}
```

Eksempelutdata:
```
Fant avsnitt: Hei, verden!
```

Dette eksempelet fokuserer på å trekke ut tekst innenfor avsnittstagger, men `libxml2` tilbyr robust støtte for å navigere og forespørre forskjellige deler av et HTML-dokument.

## Dypdykk

Parsing av HTML i C går tilbake til de tidlige dagene av webutviklingen. I begynnelsen måtte utviklere stole på egentilpassede, ofte grunnleggende parsingsløsninger, på grunn av mangel på standardiserte biblioteker og den kaotiske tilstanden til HTML på nettet. Introduksjonen av biblioteker som `libxml2` markerte en betydelig fremgang, som tilbyr mer standardiserte, effektive og motstandsdyktige tilnærminger til HTML-parsing.

Til tross for Cs uovertrufne hastighet og kontroll, er det verdt å merke seg at C kanskje ikke alltid er det beste verktøyet for parsing av HTML, spesielt for oppgaver som krever raske utviklingssykluser eller håndtering av eksepsjonelt dårlig formulert HTML. Språk med høynivå HTML-parsebiblioteker, som Python med Beautiful Soup, tilbyr mer abstraherte, brukervennlige grensesnitt på bekostning av noe ytelse.

Likevel, for ytelseskritiske applikasjoner, eller når man opererer i ressursbegrensede miljøer, forblir parsing av HTML i C en levedyktig og ofte foretrukket metode. Nøkkelen er å utnytte robuste biblioteker som `libxml2` for å håndtere HTMLs intrikate detaljer, slik at utviklere kan fokusere på å trekke ut dataene de trenger uten å bli hindret i detaljene i parsemekanismen.
