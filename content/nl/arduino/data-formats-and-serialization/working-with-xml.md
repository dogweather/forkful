---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:09.192811-07:00
description: "Werken met XML op Arduino omvat het parsen en manipuleren van XML-gegevens,\
  \ die meestal afkomstig zijn van web-API's of configuratiebestanden.\u2026"
lastmod: '2024-03-13T22:44:51.095055-06:00'
model: gpt-4-0125-preview
summary: "Werken met XML op Arduino omvat het parsen en manipuleren van XML-gegevens,\
  \ die meestal afkomstig zijn van web-API's of configuratiebestanden.\u2026"
title: Werken met XML
weight: 40
---

## Wat & Waarom?
Werken met XML op Arduino omvat het parsen en manipuleren van XML-gegevens, die meestal afkomstig zijn van web-API's of configuratiebestanden. Programmeurs doen dit om te integreren met diensten die XML gebruiken voor gegevensuitwisseling of om gegevens op te slaan in een gestructureerd, menselijk leesbaar formaat.

## Hoe te:
We gaan de `XMLWriter`-bibliotheek gebruiken om XML te creëren en de `tinyxml2`-bibliotheek om het te parsen. Installeer eerst de bibliotheken via de Bibliotheekbeheerder in je Arduino IDE.

Een XML-document creëren:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Gebruikmakend van Serial voor uitvoer
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hallo, wereld!").close().close();
  xml.flush();
}

void loop() {
}
```

Een XML-string decoderen:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hallo, wereld!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Voorbeelduitvoer:

```
<greeting>
  <text>Hallo, wereld!</text>
</greeting>
```

## Diepgaand Onderzoek
XML, of Extensible Markup Language, is een opmaaktaal die een reeks regels definieert voor het coderen van documenten in een formaat dat zowel voor mensen leesbaar als voor machines leesbaar is. Het bestaat al sinds de late jaren '90 en wordt uitgebreid gebruikt in diverse velden, vooral waar platformonafhankelijke gegevensuitwisseling nodig is. Arduino's beperkte geheugenbronnen maken het werken met XML uitdagender dan op een PC. Daarom zijn lichtgewicht bibliotheken cruciaal. Hoewel JSON aan populariteit heeft gewonnen voor gegevensuitwisseling vanwege zijn eenvoudigere syntax en kleinere voetafdruk, wordt XML nog steeds veel gebruikt, vooral bij omgang met legacy-systemen of applicaties die documentvalidatie vereisen via schema's. Key tot Arduino XML-implementatie is stream-parsing, dat het document in segmenten leest om het geheugengebruik laag te houden.

## Zie Ook
- [TinyXML-2 Bibliotheekdocumentatie](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON-Bibliotheek](https://arduinojson.org/) als alternatief bij het werken met JSON-gegevens.
- [W3Schools XML-Tutorial](https://www.w3schools.com/xml/) voor algemene XML-leer.
- [W3C XML-Specificatie](https://www.w3.org/XML/) voor de officiële XML-standaarden en aanbevelingen.
