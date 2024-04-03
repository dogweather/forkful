---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:29.721346-07:00
description: "Werken met XML betekent het parseren, cre\xEBren en manipuleren van\
  \ XML (eXtensible Markup Language) gegevens. Programmeurs beheren XML om gestructureerde\u2026"
lastmod: '2024-03-13T22:44:51.137740-06:00'
model: gpt-4-0125-preview
summary: "Werken met XML betekent het parseren, cre\xEBren en manipuleren van XML\
  \ (eXtensible Markup Language) gegevens."
title: Werken met XML
weight: 40
---

## Wat & Waarom?
Werken met XML betekent het parseren, creëren en manipuleren van XML (eXtensible Markup Language) gegevens. Programmeurs beheren XML om gestructureerde gegevensuitwisseling, configuratie en meer te hanteren, vanwege de platformneutrale aard ervan.

## Hoe te:
Hier is een eenvoudige manier om XML te parseren met de TinyXML-2 bibliotheek:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hallo, Wereld!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Voorbeelduitvoer:

```
Hallo, Wereld!
```

En zo creëer je een XML bestand:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Hallo, Wereld!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

Dit genereert een XML-bestand `output.xml` met de inhoud:

```xml
<?xml version="1.0"?>
<root>
    <message>Hallo, Wereld!</message>
</root>
```

## Diepgaande Verkenning
XML is sinds de late jaren '90 cruciaal geweest in webservices en gegevensopslag. Hoewel JSON en YAML nu gebruikelijker zijn voor configuratie en interoperabiliteit, is XML nog steeds enorm in veel ondernemingssystemen. XML parseren in C++ kan ouderwets aanvoelen met handmatige DOM/SAX parsing. Gelukkig vereenvoudigen bibliotheken zoals TinyXML-2 het. C++ heeft geen ingebouwde ondersteuning voor XML; bibliotheken zoals TinyXML-2, pugixml of Xerces pakken de lastige bits aan.

## Zie Ook
- TinyXML-2 Documentatie: https://leethomason.github.io/tinyxml2/
- pugixml bibliotheek: https://pugixml.org/
- Xerces-C++ Parser: https://xerces.apache.org/xerces-c/
- W3C XML Specificatie: https://www.w3.org/XML/
