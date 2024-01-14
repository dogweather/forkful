---
title:                "C++: Å tolke html"
simple_title:         "Å tolke html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

 Om du noen gang har jobbet med web design, nettutvikling eller datainnhenting, så har du sannsynligvis kommet over HTML-koding. HTML er det språket som brukes til å lage nettsider, og er dermed utrolig viktig for alle som jobber med teknologi. Men hva om du ønsker å hente ut spesifikk informasjon fra en nettside, slik som overskrifter eller prislisten til en nettbutikk? Her kommer parsing av HTML inn i bildet.

## Slik gjør du det

For å kunne parse HTML, trenger du et programmeringsspråk som støtter dette. For denne guiden vil vi fokusere på hvordan dette kan gjøres i C++. Det første du trenger er et parserverktøy, som for eksempel "libxml2". Deretter kan du kode følgende i et nytt C++-prosjekt:

```C++
#include <iostream>
#include <libxml/parser.h>
using namespace std;

int main() {

   // Åpne nettsiden du ønsker å parse
    string nettside = "https://www.example.com";
    
    // Opprett en ny XML-parser
    htmlParserCtxtPtr parser = htmlNewParserCtxt();
    
    // Les inn dokumentet
    xmlDocPtr dokument = htmlCtxtReadFile(parser, nettside.c_str(), NULL, HTML_PARSE_NOBLANKS | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING| HTML_PARSE_NONET);
    
    // Hent ut tittel og skriv ut
    xmlChar *tittel = (xmlChar *)dokument->name;
    cout << "Tittel: " << tittel << endl;
    
    // Frigjør minne
    xmlFreeDoc(dokument);
    xmlCleanupParser();
    return 0;
}
```

Koden over vil parse nettsiden du har valgt og skrive ut tittelen på siden til terminalen din. Merk at dette bare er et enkelt eksempel, og du kan parse og hente ut mye mer informasjon fra en nettside ved å bruke riktig metoder og funksjoner.

## Dypdykk

For å virkelig forstå hvordan parsing av HTML fungerer, må du ha kjennskap til hvordan HTML-koder er strukturert og hva de ulike kodene betyr. HTML består av ulike elementer, som for eksempel "head" som inneholder metadata om siden, og "body" som inneholder selve innholdet på siden. For å kunne hente ut informasjon, må man bruke forskjellige metoder og funksjoner for å navigere gjennom disse elementene og finne det man er ute etter.

En annen viktig ting å huske på er at nettsider kan endre seg, og dermed kan også strukturen av HTML-koden endre seg. Dette kan føre til utfordringer når man skal parse og hente ut informasjon fra en nettside. Det er derfor viktig å forstå hvordan man kan håndtere endringer og holde parseren oppdatert.

## Se også 

- [libxml2 biblioteket](https://xmlsoft.org/)
- [HTML-tag referanse](https://www.w3schools.com/tags/default.asp)
- [C++ ressurser](https://www.learncpp.com/)