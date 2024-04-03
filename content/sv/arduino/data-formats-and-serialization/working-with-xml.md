---
date: 2024-01-26 04:27:46.655105-07:00
description: "Hur man g\xF6r: Vi kommer att anv\xE4nda `XMLWriter`-biblioteket f\xF6\
  r att skapa XML och `tinyxml2`-biblioteket f\xF6r att tolka det. Installera f\xF6\
  rst biblioteken\u2026"
lastmod: '2024-03-13T22:44:38.191453-06:00'
model: gpt-4-0125-preview
summary: "Vi kommer att anv\xE4nda `XMLWriter`-biblioteket f\xF6r att skapa XML och\
  \ `tinyxml2`-biblioteket f\xF6r att tolka det."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Vi kommer att använda `XMLWriter`-biblioteket för att skapa XML och `tinyxml2`-biblioteket för att tolka det. Installera först biblioteken via Bibliotekshanteraren i din Arduino IDE.

Skapa ett XML-dokument:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Använder Serial för utmatning
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hej, världen!").close().close();
  xml.flush();
}

void loop() {
}
```

Dekodera en XML-sträng:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hej, världen!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Exempelutmatning:

```
<greeting>
  <text>Hej, världen!</text>
</greeting>
```

## Djupdykning
XML, eller Extensible Markup Language, är ett märkspråk som definierar en uppsättning regler för att koda dokument i ett format som är både mänskligt läsbart och maskinläsbart. Det har funnits sedan slutet av 90-talet och används flitigt inom olika områden, speciellt där plattformsoberoende datautbyte behövs. Arduinos begränsade minnesresurser gör att arbete med XML är mer utmanande än på en PC. Därför är lätta bibliotek avgörande. Även om JSON har blivit populärt för datautbyte på grund av sin enklare syntax och mindre fotavtryck, används XML fortfarande brett, särskilt när man hanterar äldre system eller applikationer som kräver dokumentvalidering via scheman. Nyckeln till Arduinos XML-implementering är strömanalys, som läser dokumentet i segment för att hålla minnesanvändningen låg.

## Se även
- [TinyXML-2 Library Documentation](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON Library](https://arduinojson.org/) som ett alternativ när man arbetar med JSON-data.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/) för allmän lärande om XML.
- [W3C XML Specification](https://www.w3.org/XML/) för officiella XML-standarder och rekommendationer.
