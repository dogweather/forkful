---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:30:20.372485-07:00
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
HTML:n jäsentäminen eli parsing tarkoittaa HTML-dokumentin rakenteen lukemista ja sen sisällön muuttamista käsiteltävään muotoon. Ohjelmoijat jäsentävät HTML:ää, koska sen avulla voidaan automatisoida tietosisällön käsittelyä, validoida HTML-koodia tai vaikkapa kaapia dataa web-sivuilta.

## How to (Kuinka?):
C-kielessä HTML:n jäsentäminen vaatii ulkoisen kirjaston, esimerkkinä voidaan käyttää `libxml2`, joka on laajalti käytössä oleva jäsentämiskirjasto.

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc; // HTML-dokumentin käsittelyyn tarkoitettu osoitin
    htmlNodePtr cur; // Solmu rakenteessa, alkio jota käsitellään
  
    // Parse HTML document from a string (tätä voisi olla esim. HTTP-vastauksessa)
    char *htmlContent = "<html><body><p>Hello, Finland!</p></body></html>";
    doc = htmlReadDoc((xmlChar*)htmlContent, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
  
    // Käydään läpi dokumentin solmut
    cur = xmlDocGetRootElement(doc);
    while (cur != NULL) {
        if (cur->type == XML_ELEMENT_NODE) {
            printf("Elementti '%s'\n", cur->name);
        }
        cur = cur->next;
    }

    // Vapautetaan dokumentin muisti
    xmlFreeDoc(doc);
  
    return 0;
}
```

Sample output (Otostuloste):
```
Elementti 'html'
Elementti 'body'
Elementti 'p'
```

## Deep Dive (Sukellus syvemmälle):
HTML:n jäsentäminen C-kielellä ei ole ihan yhtä suoraviivaista kuin jotkut korkeamman tason kielet, kuten Python tai JavaScript tarjoavat. Historiallisesti C-kieltä ei ole suunniteltu verkko-ohjelmointiin, mutta monipuolisten kirjastojen myötä se on mahdollista.

Alternatiiveina `libxml2`:lle voi mainita `Gumbo` -parserin, joka on Googlen kehittämä HTML5:n jäsentämiseen.

Jäsentämisessä tärkeää on tunnistaa elementtien hierarkia ja attribuutit. `libxml2` perustuu DOM (Document Object Model) -pohjaiseen käsittelyyn, missä koko dokumentti ladataan muistiin ja jäsentämisen jälkeen dokumentti voidaan käsitellä olio-rakenteena.

## See Also (Katso myös):
- `libxml2` dokumentaatio: http://xmlsoft.org/
- `Gumbo` parser: https://github.com/google/gumbo-parser
- HTML standardi: https://html.spec.whatwg.org/
- C standardikirjasto: http://www.iso-9899.info/wiki/The_Standard

Jos aihe jäi kiinnostamaan, näissä linkeissä on lisätietoa ja syventävää materiaalia.
