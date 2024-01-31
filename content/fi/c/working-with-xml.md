---
title:                "XML:n käsittely"
date:                  2024-01-26T04:28:11.214176-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
XML:n käsittely C-kielessä sisältää XML-tiedostojen jäsennyksen, luomisen ja manipuloinnin - periaatteessa rakenteisen datan tallennusta. Ohjelmoijat tekevät tämän vuorovaikutukseen datan kanssa kannettavassa ja ihmisen luettavassa muodossa, jota usein käytetään konfiguraatioon, datan vaihtoon ja muuhun.

## Kuinka:
Alla on pätkä, joka käyttää `libxml2` kirjastoa XML-tiedoston jäsennykseen ja juurielementin hakemiseen.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // Jäsennä XML-tiedosto
    doc = xmlReadFile("example.xml", NULL, 0);

    // Hae juurielementti
    root_element = xmlDocGetRootElement(doc);

    printf("Juurielementti: %s\n", root_element->name);

    // Vapauta dokumentti
    xmlFreeDoc(doc);

    // Siivoa jäsennin
    xmlCleanupParser();

    return 0;
}
```

Esimerkkituloste XML:lle, jonka juuri on `<data>`:
```
Juurielementti: data
```

## Syvemmälle
XML eli Laajennettava Merkintäkieli juontaa juurensa 90-luvun lopulle tarjoten tavan kuvailla ja rakentaa dataa. C:ssä `libxml2` on se mihin useimmiten turvaudutaan. Se on robusti, mutta ei helpoin XML-aloittelijoille. Vaihtoehtoihin kuuluu `tinyxml2`, joka on kevyempi ja aloittelijaystävällisempi. Toteutuksen kannalta, C:ssä ei ole sisäänrakennettua XML-tukea, joten kirjastot täyttävät tämän aukon. Ne vaihtelevat kooltaan, nopeudeltaan, monimutkaisuudeltaan ja siirrettävyydeltään. Useimmat tarjoavat DOM- ja SAX-jäsennysmenetelmiä: DOM lataa koko jutun muistiin, hyvä pienille dokkareille; SAX on tapahtumavetoinen, käsittelee elementtejä lennosta, parempi isoille tiedostoille. Molemmilla on käyttötarkoituksensa ja kompromissinsa.

## Katso Myös
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 GitHubissa](https://github.com/leethomason/tinyxml2)
- [XML-tutoriaali w3schools-sivustolla](https://www.w3schools.com/xml/)
- [XML-spesifikaatio W3C:ltä](https://www.w3.org/XML/)
