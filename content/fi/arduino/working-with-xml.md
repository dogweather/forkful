---
title:                "XML:n käsittely"
date:                  2024-01-26T04:27:22.157068-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Työskentely XML:n kanssa Arduinolla käsittää XML-datan jäsentämistä ja manipulointia, joka usein tulee web-API:sta tai konfiguraatiotiedostoista. Ohjelmoijat tekevät tämän integroitumiseen palveluihin, jotka käyttävät XML:ää datanvaihtoon tai tallentaakseen dataa rakenteelliseen, ihmisen luettavissa olevaan muotoon.

## Kuinka:
Käytämme `XMLWriter`-kirjastoa XML:n luontiin ja `tinyxml2`-kirjastoa sen jäsentämiseen. Asenna kirjastot ensin Kirjastohallinnan kautta Arduino IDE:ssäsi.

XML-dokumentin luonti:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Käyttäen Serialia tulostukseen
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hei, maailma!").close().close();
  xml.flush();
}

void loop() {
}
```

XML-merkkijonon purkaminen:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hei, maailma!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Esimerkkivastaus:

```
<greeting>
  <text>Hei, maailma!</text>
</greeting>
```

## Syväsukellus
XML eli Laajennettava Merkintäkieli on merkintäkieli, joka määrittelee joukon sääntöjä dokumenttien koodaamiseen muodossa, joka on sekä ihmisen lueteltavissa että koneellisesti luettavissa. Se on ollut olemassa 90-luvun lopulta lähtien ja sitä käytetään laajasti eri aloilla, erityisesti alustoista riippumattoman datanvaihdon tarpeisiin. Arduinon rajalliset muistiresurssit tekevät XML:n käsittelystä haastavampaa kuin PC:llä. Tästä syystä kevyet kirjastot ovat ratkaisevan tärkeitä. Vaikka JSON on saavuttanut suosiota datanvaihdossa sen yksinkertaisemman syntaksin ja pienemmän jalanjäljen ansiosta, XML:ää käytetään edelleen laajasti, erityisesti kun työskennellään legacy-järjestelmien kanssa tai sovelluksissa, jotka vaativat dokumentin validointia skeemojen kautta. Avain Arduino XML-toteutuksessa on suoratoistojäsentäminen, joka lukee asiakirjan osissa pitääkseen muistin käytön alhaisena.

## Katso Myös
- [TinyXML-2 -kirjaston dokumentaatio](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON -kirjasto](https://arduinojson.org/) vaihtoehtona, kun työskennellään JSON-datan kanssa.
- [W3Schoolsin XML-oppitunti](https://www.w3schools.com/xml/) yleiseen XML:n oppimiseen.
- [W3C:n XML-spesifikaatio](https://www.w3.org/XML/) virallisille XML-standardeille ja suosituksille.