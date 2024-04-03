---
date: 2024-01-26 04:28:24.856024-07:00
description: "C\xF3mo hacerlo: Aqu\xED hay una manera simple de analizar XML utilizando\
  \ la biblioteca TinyXML-2."
lastmod: '2024-03-13T22:44:59.399336-06:00'
model: gpt-4-0125-preview
summary: "Aqu\xED hay una manera simple de analizar XML utilizando la biblioteca TinyXML-2."
title: Trabajando con XML
weight: 40
---

## Cómo hacerlo:
Aquí hay una manera simple de analizar XML utilizando la biblioteca TinyXML-2:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hola, Mundo!</message></root>");
    const char* contenido = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << contenido << std::endl;
    return 0;
}
```

Salida de ejemplo:

```
Hola, Mundo!
```

Y así es cómo creas un archivo XML:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaracion = doc.NewDeclaration();
    doc.InsertFirstChild(declaracion);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* mensaje = doc.NewElement("message");
    mensaje->SetText("Hola, Mundo!");
    root->InsertEndChild(mensaje);
    doc.SaveFile("salida.xml");
    return 0;
}
```

Esto genera un archivo XML `salida.xml` con los contenidos:

```xml
<?xml version="1.0"?>
<root>
    <message>Hola, Mundo!</message>
</root>
```

## Análisis Profundo
XML ha sido pivotal en los servicios web y el almacenamiento de datos desde finales de los '90. Aunque JSON y YAML son ahora más comunes para configuración e interoperabilidad, XML sigue siendo enorme en muchos sistemas empresariales. Analizar XML en C++ puede sentirse anticuado con el análisis DOM/SAX manual. Afortunadamente, bibliotecas como TinyXML-2 lo simplifican. C++ no tiene soporte integrado para XML; bibliotecas como TinyXML-2, pugixml o Xerces solventan las partes difíciles.

## Ver También
- Documentación de TinyXML-2: https://leethomason.github.io/tinyxml2/
- biblioteca pugixml: https://pugixml.org/
- Analizador Xerces-C++: https://xerces.apache.org/xerces-c/
- Especificación XML de W3C: https://www.w3.org/XML/
