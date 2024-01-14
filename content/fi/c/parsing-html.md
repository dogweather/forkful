---
title:                "C: Tiedostojen jäsentäminen"
simple_title:         "Tiedostojen jäsentäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan miettinyt, miten internetsivut todellisuudessa toimivat? Vaikka meidän silmämme näkevät vain näyttävän ulkoasun ja sisällön, internetsivut ovat todellisuudessa yhtä kuin massiivinen määrä tekstiä, joka tunteina taustalla pyörii. Tämä teksti tunnetaan HTML:änä, ja sen parsiminen on monimutkainen, mutta olennainen osa nettisivujen latautumista ja toimintaa.

Tämän blogin tarkoituksena on selittää, miksi HTML:n parsiminen on tärkeää ja miten se tapahtuu C-kielellä.

## Miten

HTML-niminen kieli on koodattu yksinkertaisen rakenteen avulla, jossa elementit koostuvat avaus- ja sulkutageista. Näiden tagien välissä on muuta sisältöä, kuten tekstiä, kuvia tai linkkejä. Jotta nettisivu saadaan näyttämään kauniilta ja käyttäjäystävälliseltä, nämä elementit täytyy pystyä hahmottamaan ja tekemään niistä tyylikkäitä ja toimivia.

Onneksi C-kielellä on helppo käyttää HTML:n parsimiseen tarkoitettuja kirjastoja, kuten libxml2 ja Gumbo. Nämä kirjastot antavat ohjelmoijalle valmiin työkalun, jolla HTML-tiedostosta voidaan helposti etsiä ja poimia haluttuja elementtejä.

Alla on esimerkki, jossa käytämme libxml2-kirjastoa prosessoimaan yksinkertaista HTML-tiedostoa:

```C
#include <stdlib.h>
#include <stdio.h>
#include <libxml2/libxml2.h>

int main() {
    xmlParserCtxtPtr context = xmlNewParserCtxt();
    xmlDocPtr doc = xmlCtxtReadFile(context, "index.html", NULL, XML_PARSE_RECOVER);
    xmlNodePtr root = xmlDocGetRootElement(doc);
    
    xmlNodePtr body = root->children->next;
    
    xmlNodePtr h1 = body->children;
    
    xmlChar *string = xmlNodeGetContent(h1);
    
    printf("Welcome to my website, %s!\n", (char*) string);
    
    xmlFree(string);
    xmlFreeDoc(doc);
    xmlFreeParserCtxt(context);
    
    return 0;
}
```

Tulostaa:

```
Welcome to my website, HTML-parsimisesimerkki!
```

## Syvemmälle

HTML:n parsimisessa on kuitenkin paljon monimutkaisempia ja hienovaraisempia seikkoja, kuten virheiden käsittely, CSS:n huomioon ottaminen ja sivujen dynaaminen generointi. Nämä ovatkin syitä, miksi HTML:n parsiminen voi olla haastavaa ja aikaa vievää.

Yksi tapa tehdä HTML:n parsimisesta helpompaa ja tehokkaampaa on käyttää CSS-selektoreita. Nämä ovat erityisiä merkkijonoja, joiden avulla voidaan määrittää tietyn elementin sijainti sivulla. Tämä vähentää tarvetta käyttää monimutkaisia hakuja ja puumaisia tietorakenteita.

## Katso myös

- [Libxml2 kirjasto C-kielelle](http://www.xmlsoft.org/)
- [Gumbo kirjasto C-kielelle](https://github.com/google/gumbo-parser)
- [HTML:n parsimisen perusteet](https://www.w3schools.com/tags/tag_doctype.asp)