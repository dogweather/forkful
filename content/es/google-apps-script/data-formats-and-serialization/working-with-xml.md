---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:25.301285-07:00
description: "Trabajar con XML en Google Apps Script permite a los programadores analizar,\
  \ manipular y generar datos XML, esencial para servicios web y configuraciones.\u2026"
lastmod: '2024-03-13T22:44:58.591957-06:00'
model: gpt-4-0125-preview
summary: Trabajar con XML en Google Apps Script permite a los programadores analizar,
  manipular y generar datos XML, esencial para servicios web y configuraciones.
title: Trabajando con XML
weight: 40
---

## ¿Qué y por qué?

Trabajar con XML en Google Apps Script permite a los programadores analizar, manipular y generar datos XML, esencial para servicios web y configuraciones. Los programadores adoptan este enfoque para integrarse con sistemas heredados, realizar scraping web o comunicarse con numerosas API que todavía dependen de XML sobre JSON para el intercambio de datos.

## Cómo:

Google Apps Script proporciona el `XmlService` para trabajar con datos XML. A continuación, demostramos cómo analizar una cadena XML, modificar su contenido y generar una nueva cadena XML.

Analizando una cadena XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Registra: Hello
}
```

Para modificar el XML, es posible que desee agregar un nuevo elemento hijo:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Registra la nueva cadena XML con el elemento hijo agregado
}
```

Generando una cadena XML desde cero:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Salida: <root><child>Hello World</child></root>
}
```

## Análisis profundo

Históricamente, XML (Lenguaje de Marcado Extensible) fue el estándar de facto para el intercambio de datos antes de que JSON surgiera como una alternativa ligera. La sintaxis verbosa de XML y su estricto modelo de análisis proporcionaron un formato de datos robusto, aunque voluminoso. En Google Apps Script, la API `XmlService` encapsula la creación, análisis y manipulación de datos XML, reconociendo su importancia continua en varios sistemas heredados y empresariales, servicios web SOAP y archivos de configuración para aplicaciones.

A pesar de la prevalencia de JSON en el desarrollo web moderno por su simplicidad y facilidad de uso con JavaScript, XML sigue siendo relevante en áreas donde la validación de documentos y las jerarquías estructuradas son cruciales. Sin embargo, para nuevos proyectos, especialmente aquellos inclinados hacia las API web, JSON a menudo es la opción más práctica debido a su naturaleza ligera y su integración perfecta con JavaScript.

Entender XML y su manejo en Google Apps Script es primordial para los desarrolladores que trabajan en entornos donde la integración con sistemas más antiguos o API empresariales específicas es necesaria. Sin embargo, al iniciar nuevos proyectos o cuando la flexibilidad es clave, es aconsejable evaluar la necesidad de XML sobre alternativas como JSON.
