---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsennys on prosessi, jossa HTML-teksti muutetaan rakenteelliseksi muodoksi, jolla ohjelmat voivat käsitellä tiedot. Ohjelmoijat tekevät sen, jotta voivat käyttää HTML:stä saatavia tietoja kolmannen osapuolen tarkoituksiin.

## Kuinka se tehdään:

Esimerkinä libxml2-kirjaston käyttö C-kielellä. Tarkastelemme HTML-tiedostoa ja siirrämme sen tiedot rakenteisiin.

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc = htmlReadFile("esimerkki.html", NULL, HTML_PARSE_RECOVER);
    xmlNodePtr node = xmlDocGetRootElement(doc);

    while (node != NULL) {
        printf("Elementin nimi: %s\n", node->name);
        node = node->next;
    }

    xmlFreeDoc(doc);

    return 0;
}
```

Käynnistettäessä yllä oleva ohjelma, se tulostaa HTML-dokumentin esimerkki.html kaikkien elementtien nimet konsoliin.

## Syvempi sukellus

HTML-jäsentämistä on käytetty Webin alkuaikoina luomaan Web-robotteja ja -indeksoijia, jotka lukevat ja indeksoivat verkkosivuja. Nykyään sitä käytetään erilaisiin sovelluksiin, kuten datan kaivamiseen verkkosivuilta tai käyttöliittymien automatisointiin.

On olemassa erilaisia tapoja jäsennys HTML:ää. Esimerkki, jonka annoimme, käyttää libxml2-kirjastoa. Muita vaihtoehtoja on olemassa, kuten BeautifulSoup tai lxml, mutta ne ovat saatavilla Pythonille, eivät C:lle.

HTML-jäsentämisen toteutuksen oleellinen yksityiskohta liittyy DOM:rakenteeseen (Document Object Model), joka on järjestetty puurakenne, jonka HTML-elementit ja niiden ominaisuudet muodostavat.

## Katso myös

1. Libxml2-dokumentaatio: http://xmlsoft.org/html/libxml-HTMLparser.html
2. C-kielen oppimateriaali: https://www.learn-c.org/
3. Document Object Model (DOM): https://www.w3.org/TR/DOM-Level-2-Core/introduction.html