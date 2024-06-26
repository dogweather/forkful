---
date: 2024-01-26 04:32:26.591940-07:00
description: "C\xF3mo hacerlo: Aqu\xED le mostramos c\xF3mo analizar XML."
lastmod: '2024-03-13T22:44:59.482674-06:00'
model: gpt-4-0125-preview
summary: "Aqu\xED le mostramos c\xF3mo analizar XML."
title: Trabajando con XML
weight: 40
---

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
