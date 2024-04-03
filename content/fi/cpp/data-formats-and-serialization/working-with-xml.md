---
date: 2024-01-26 04:28:24.545351-07:00
description: "XML:n (laajennettava merkint\xE4kieli) kanssa ty\xF6skentely tarkoittaa\
  \ XML-tietojen j\xE4sent\xE4mist\xE4, luomista ja manipulointia. Ohjelmoijat hallinnoivat\
  \ XML-\u2026"
lastmod: '2024-03-13T22:44:56.889960-06:00'
model: gpt-4-0125-preview
summary: "XML:n (laajennettava merkint\xE4kieli) kanssa ty\xF6skentely tarkoittaa\
  \ XML-tietojen j\xE4sent\xE4mist\xE4, luomista ja manipulointia."
title: "XML:n k\xE4sittely"
weight: 40
---

## Mikä ja miksi?
XML:n (laajennettava merkintäkieli) kanssa työskentely tarkoittaa XML-tietojen jäsentämistä, luomista ja manipulointia. Ohjelmoijat hallinnoivat XML-tietoja käsitelläkseen rakenteistettua datanvaihtoa, konfiguraatiota ja muuta sen alusta-neutraalin luonteen vuoksi.

## Kuinka:
Tässä on yksinkertainen tapa jäsentää XML käyttäen TinyXML-2-kirjastoa:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hei, maailma!</message></root>");
    const char* sisältö = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << sisältö << std::endl;
    return 0;
}
```

Esimerkkituloste:

```
Hei, maailma!
```

Ja näin luot XML-tiedoston:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* julistus = doc.NewDeclaration();
    doc.InsertFirstChild(julistus);
    auto* juuri = doc.NewElement("root");
    doc.InsertEndChild(juuri);
    auto* viesti = doc.NewElement("message");
    viesti->SetText("Hei, maailma!");
    juuri->InsertEndChild(viesti);
    doc.SaveFile("tulos.xml");
    return 0;
}
```

Tämä luo XML-tiedoston `tulos.xml` sisällöllä:

```xml
<?xml version="1.0"?>
<root>
    <message>Hei, maailma!</message>
</root>
```

## Syväsukellus
XML on ollut keskeisessä osassa web-palveluissa ja datan tallennuksessa 90-luvun loppupuolelta lähtien. Vaikka JSON ja YAML ovat nykyään yleisempiä konfiguraatioon ja yhteentoimivuuteen, XML on edelleen valtava monissa yritysjärjestelmissä. XML:n jäsentäminen C++:ssa voi tuntua vanhanaikaiselta manuaalisen DOM/SAX-jäsentämisen kanssa. Onneksi kirjastot kuten TinyXML-2 yksinkertaistavat sitä. C++:ssa ei ole sisäänrakennettua XML-tukea; kirjastot kuten TinyXML-2, pugixml tai Xerces kääriytyvät vaikeiden osien ympärille.

## Katso myös
- TinyXML-2-dokumentaatio: https://leethomason.github.io/tinyxml2/
- pugixml-kirjasto: https://pugixml.org/
- Xerces-C++ -jäsennin: https://xerces.apache.org/xerces-c/
- W3C XML-spesifikaatio: https://www.w3.org/XML/
