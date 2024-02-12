---
title:                "Trabajando con XML"
aliases:
- /es/arduino/working-with-xml.md
date:                  2024-01-26T04:27:22.145456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con XML en Arduino involucra analizar y manipular datos XML, que usualmente provienen de APIs web o archivos de configuración. Los programadores lo hacen para integrarse con servicios que usan XML para el intercambio de datos o para almacenar datos en un formato estructurado y legible por humanos.

## Cómo hacerlo:
Usaremos la biblioteca `XMLWriter` para crear XML y la biblioteca `tinyxml2` para analizarlo. Primero instala las bibliotecas a través del Gestor de Bibliotecas en tu IDE de Arduino.

Creando un documento XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Usando Serial para la salida
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hola, mundo!").close().close();
  xml.flush();
}

void loop() {
}
```

Decodificando una cadena XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hola, mundo!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Ejemplo de salida:

```
<greeting>
  <text>Hola, mundo!</text>
</greeting>
```

## Análisis Profundo
XML, o Lenguaje de Marcado Extensible, es un lenguaje de marcado que define un conjunto de reglas para codificar documentos en un formato que es legible tanto por humanos como por máquinas. Ha existido desde finales de los años 90 y se usa extensamente en varios campos, especialmente donde se necesita un intercambio de datos independiente de la plataforma. Los recursos limitados de memoria de Arduino hacen que trabajar con XML sea más desafiante que en una PC. Por lo tanto, las bibliotecas ligeras son cruciales. Aunque JSON ha ganado popularidad para el intercambio de datos debido a su síntaxis más simple y menor huella, XML todavía se usa ampliamente, especialmente cuando se trata de sistemas heredados o aplicaciones que requieren validación de documentos a través de esquemas. La clave para la implementación de XML en Arduino es el análisis de flujo, que lee el documento en segmentos para mantener bajo el uso de memoria.

## Ver También
- [Documentación de la Biblioteca TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Biblioteca Arduino JSON](https://arduinojson.org/) como alternativa cuando se trabaja con datos JSON.
- [Tutorial de XML de W3Schools](https://www.w3schools.com/xml/) para aprender sobre XML en general.
- [Especificación XML de W3C](https://www.w3.org/XML/) para los estándares y recomendaciones oficiales de XML.
