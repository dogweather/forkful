---
title:                "Trabajando con XML"
date:                  2024-01-26T04:28:24.856024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con XML significa analizar, crear y manipular datos XML (Lenguaje de Marcado Extensible). Los programadores manejan XML para gestionar el intercambio de datos estructurados, la configuración y más, debido a su naturaleza neutral en cuanto a la plataforma.

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