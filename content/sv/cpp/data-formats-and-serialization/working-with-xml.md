---
date: 2024-01-26 04:28:31.619570-07:00
description: "Hur man g\xF6r: H\xE4r \xE4r ett enkelt s\xE4tt att tolka XML med hj\xE4\
  lp av TinyXML-2-biblioteket."
lastmod: '2024-03-13T22:44:38.233789-06:00'
model: gpt-4-0125-preview
summary: "H\xE4r \xE4r ett enkelt s\xE4tt att tolka XML med hj\xE4lp av TinyXML-2-biblioteket."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Här är ett enkelt sätt att tolka XML med hjälp av TinyXML-2-biblioteket:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hej, världen!</message></root>");
    const char* innehåll = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << innehåll << std::endl;
    return 0;
}
```

Exempelutskrift:

```
Hej, världen!
```

Och så här skapar du en XML-fil:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* deklaration = doc.NewDeclaration();
    doc.InsertFirstChild(deklaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* meddelande = doc.NewElement("message");
    meddelande->SetText("Hej, världen!");
    root->InsertEndChild(meddelande);
    doc.SaveFile("output.xml");
    return 0;
}
```

Detta genererar en XML-fil `output.xml` med innehållet:

```xml
<?xml version="1.0"?>
<root>
    <message>Hej, världen!</message>
</root>
```

## Fördjupning
XML har varit avgörande för webbtjänster och datalagring sedan slutet av 90-talet. Medan JSON och YAML nu är mer vanliga för konfiguration och interoperabilitet, är XML fortfarande stort i många företagssystem. Att tolka XML i C++ kan kännas gammaldags med manuell DOM/SAX-tolkning. Lyckligtvis förenklar bibliotek som TinyXML-2 det. C++ har inget inbyggt XML-stöd; bibliotek som TinyXML-2, pugixml eller Xerces paketerar de svåra bitarna.

## Se även
- TinyXML-2-dokumentation: https://leethomason.github.io/tinyxml2/
- pugixml-bibliotek: https://pugixml.org/
- Xerces-C++-tolk: https://xerces.apache.org/xerces-c/
- W3Cs XML-specifikation: https://www.w3.org/XML/
