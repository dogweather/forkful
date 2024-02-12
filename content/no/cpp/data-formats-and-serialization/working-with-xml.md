---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:28:25.436705-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å jobbe med XML betyr analyse, oppretting og manipulering av XML (eXtensible Markup Language)-data. Programmerere håndterer XML for å håndtere strukturert datautveksling, konfigurasjon og mer, på grunn av dens plattformnøytrale natur.

## Hvordan:
Her er en enkel måte å analysere XML på ved hjelp av TinyXML-2-biblioteket:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hei, Verden!</message></root>");
    const char* innhold = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << innhold << std::endl;
    return 0;
}
```

Eksempelutskrift:

```
Hei, Verden!
```

Og slik oppretter du en XML-fil:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* deklarasjon = doc.NewDeclaration();
    doc.InsertFirstChild(deklarasjon);
    auto* rot = doc.NewElement("root");
    doc.InsertEndChild(rot);
    auto* melding = doc.NewElement("message");
    melding->SetText("Hei, Verden!");
    rot->InsertEndChild(melding);
    doc.SaveFile("output.xml");
    return 0;
}
```

Dette genererer en XML-fil `output.xml` med innhold:

```xml
<?xml version="1.0"?>
<root>
    <message>Hei, Verden!</message>
</root>
```

## Dypdykk
XML har vært avgjørende for webtjenester og datalagring siden slutten av '90-tallet. Selv om JSON og YAML nå er mer vanlige for konfig og interop, er XML fortsatt enormt i mange virksomhetssystemer. Å analysere XML i C++ kan føles gammeldags med manuell DOM/SAX-analyse. Heldigvis forenkler biblioteker som TinyXML-2 dette. C++ har ingen innebygd XML-støtte; biblioteker som TinyXML-2, pugixml eller Xerces pakker sammen de vanskelige bitene.

## Se Også
- TinyXML-2 Dokumentasjon: https://leethomason.github.io/tinyxml2/
- pugixml-biblioteket: https://pugixml.org/
- Xerces-C++ Parser: https://xerces.apache.org/xerces-c/
- W3C XML-spesifikasjon: https://www.w3.org/XML/
