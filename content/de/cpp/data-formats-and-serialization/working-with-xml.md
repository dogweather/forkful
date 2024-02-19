---
aliases:
- /de/cpp/working-with-xml/
date: 2024-01-26 04:28:13.508855-07:00
description: "Mit XML zu arbeiten bedeutet, XML-Daten (eXtensible Markup Language)\
  \ zu parsen, zu erstellen und zu manipulieren. Programmierer verwalten XML, um\u2026"
lastmod: 2024-02-18 23:09:05.214661
model: gpt-4-0125-preview
summary: "Mit XML zu arbeiten bedeutet, XML-Daten (eXtensible Markup Language) zu\
  \ parsen, zu erstellen und zu manipulieren. Programmierer verwalten XML, um\u2026"
title: Arbeiten mit XML
---

{{< edit_this_page >}}

## Was & Warum?
Mit XML zu arbeiten bedeutet, XML-Daten (eXtensible Markup Language) zu parsen, zu erstellen und zu manipulieren. Programmierer verwalten XML, um strukturierten Datenaustausch, Konfiguration und mehr zu handhaben, aufgrund seiner plattformneutralen Natur.

## Wie:
Hier ist eine einfache Art, XML mit der TinyXML-2-Bibliothek zu parsen:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hallo, Welt!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Beispielausgabe:

```
Hallo, Welt!
```

Und so erstellen Sie eine XML-Datei:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* deklaration = doc.NewDeclaration();
    doc.InsertFirstChild(deklaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* nachricht = doc.NewElement("message");
    nachricht->SetText("Hallo, Welt!");
    root->InsertEndChild(nachricht);
    doc.SaveFile("ausgabe.xml");
    return 0;
}
```

Dadurch wird eine XML-Datei `ausgabe.xml` mit Inhalten erstellt:

```xml
<?xml version="1.0"?>
<root>
    <message>Hallo, Welt!</message>
</root>
```

## Tiefere Einblicke
XML war seit Ende der 90er Jahre entscheidend für Webdienste und Datenspeicherung. Obwohl JSON und YAML jetzt häufiger für Konfiguration und Interoperabilität verwendet werden, ist XML immer noch riesig in vielen Unternehmenssystemen. XML in C++ zu parsen, kann altmodisch wirken mit manuellem DOM/SAX-Parsing. Glücklicherweise vereinfachen Bibliotheken wie TinyXML-2 dies. C++ hat keine eingebaute XML-Unterstützung; Bibliotheken wie TinyXML-2, pugixml oder Xerces wickeln die schwierigen Teile ab.

## Siehe auch
- TinyXML-2-Dokumentation: https://leethomason.github.io/tinyxml2/
- pugixml-Bibliothek: https://pugixml.org/
- Xerces-C++-Parser: https://xerces.apache.org/xerces-c/
- W3C XML-Spezifikation: https://www.w3.org/XML/
