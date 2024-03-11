---
date: 2024-01-26 04:27:23.179307-07:00
description: "Arbeid med XML p\xE5 Arduino inneb\xE6rer parsing og manipulering av\
  \ XML-data, som vanligvis kommer fra web-APIer eller konfigurasjonsfiler. Programmerere\
  \ gj\xF8r\u2026"
lastmod: '2024-03-11T00:14:14.674124-06:00'
model: gpt-4-0125-preview
summary: "Arbeid med XML p\xE5 Arduino inneb\xE6rer parsing og manipulering av XML-data,\
  \ som vanligvis kommer fra web-APIer eller konfigurasjonsfiler. Programmerere gj\xF8\
  r\u2026"
title: "\xC5 jobbe med XML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med XML på Arduino innebærer parsing og manipulering av XML-data, som vanligvis kommer fra web-APIer eller konfigurasjonsfiler. Programmerere gjør dette for å integrere med tjenester som bruker XML for datautveksling eller for å lagre data i et strukturert, menneskelesbart format.

## Hvordan:
Vi vil bruke `XMLWriter`-biblioteket til å opprette XML og `tinyxml2`-biblioteket for å parse det. Installer først bibliotekene via Biblioteksbehandleren i ditt Arduino IDE.

Opprette et XML-dokument:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Bruker Serial for utdata
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hei, verden!").close().close();
  xml.flush();
}

void loop() {
}
```

Dekoding av en XML-streng:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hei, verden!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Eksempel på utdata:

```
<greeting>
  <text>Hei, verden!</text>
</greeting>
```

## Dypdykk
XML, eller Utvidbart Merkespråk, er et merkespråk som definerer et sett med regler for kodning av dokumenter i et format som både er menneskelesbart og maskinlesbart. Det har vært rundt siden slutten av 90-tallet og brukes utstrakt i ulike felt, særlig der det er behov for plattformuavhengig datautveksling. Arduinos begrensede minneressurser gjør arbeid med XML mer utfordrende enn på en PC. Derfor er lettvektsbiblioteker avgjørende. Selv om JSON har fått popularitet for datautveksling på grunn av sin enklere syntaks og mindre fotavtrykk, er XML fortsatt mye brukt, særlig når det gjelder eldre systemer eller applikasjoner som krever dokumentvalidering via skjemaer. Nøkkelen til Arduino XML-implementering er strømparsing, som leser dokumentet i segmenter for å holde minnebruken lav.

## Se også
- [TinyXML-2 Biblioteksdokumentasjon](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON Bibliotek](https://arduinojson.org/) for et alternativ når du arbeider med JSON data.
- [W3Schools XML Veiledning](https://www.w3schools.com/xml/) for generell læring om XML.
- [W3C XML Spesifikasjon](https://www.w3.org/XML/) for de offisielle XML-standardene og anbefalingene.
