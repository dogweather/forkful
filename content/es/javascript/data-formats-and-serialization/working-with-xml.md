---
date: 2024-01-26 04:32:26.591940-07:00
description: "Trabajar con XML significa analizar, manipular y producir contenido\
  \ XML mediante c\xF3digo. Los programadores lo hacen porque XML se utiliza ampliamente\
  \ para\u2026"
lastmod: '2024-02-25T18:49:55.952005-07:00'
model: gpt-4-0125-preview
summary: "Trabajar con XML significa analizar, manipular y producir contenido XML\
  \ mediante c\xF3digo. Los programadores lo hacen porque XML se utiliza ampliamente\
  \ para\u2026"
title: Trabajando con XML
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con XML significa analizar, manipular y producir contenido XML mediante código. Los programadores lo hacen porque XML se utiliza ampliamente para archivos de configuración, intercambio de datos y servicios web debido a su naturaleza legible por humanos y analizable por máquinas.

## Cómo hacerlo:

Aquí le mostramos cómo analizar XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Usuario</to>
                    <from>Autor</from>
                    <heading>Recordatorio</heading>
                    <body>¡No me olvides este fin de semana!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Salida: Usuario
```

Y para producir XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Usuario';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Salida: <note><to>Usuario</to></note>
```

## Profundización

XML es la abreviatura de eXtensible Markup Language, un formato de datos que existe desde finales de los 90. Define un conjunto de reglas para codificar documentos que tanto humanos como máquinas pueden leer. Históricamente, XML ganó tracción por su flexibilidad y jerarquía estructurada, convirtiéndolo en una opción para servicios web, como SOAP, y numerosos archivos de configuración.

Las alternativas a XML incluyen JSON (JavaScript Object Notation), que se ha popularizado por su facilidad de uso con JavaScript y por ser más ligero. YAML es otra alternativa, valorada por ser amigable para los humanos y una opción común para la configuración.

XML se implementa en JavaScript utilizando las interfaces DOMParser y XMLSerializer. El DOM de XML (Modelo de Objetos de Documento) permite navegar y editar documentos XML como lo haría con HTML. A pesar del ascenso de JSON, entender XML es clave, ya que numerosos sistemas heredados y ciertas industrias todavía dependen de él para el intercambio de datos.

## Ver también

- MDN Web Docs (Análisis de XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Tutorial de DOM XML): https://www.w3schools.com/xml/dom_intro.asp
- "¿Qué es XML?": https://www.w3.org/XML/
