---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:44.671514-07:00
description: "Trabajar con XML en Dart implica analizar, consultar y modificar documentos\
  \ XML, un proceso crucial para aplicaciones que interact\xFAan con servicios web,\u2026"
lastmod: '2024-03-13T22:44:58.780806-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con XML en Dart implica analizar, consultar y modificar documentos\
  \ XML, un proceso crucial para aplicaciones que interact\xFAan con servicios web,\u2026"
title: Trabajando con XML
weight: 40
---

## ¿Qué y Por Qué?

Trabajar con XML en Dart implica analizar, consultar y modificar documentos XML, un proceso crucial para aplicaciones que interactúan con servicios web, archivos de configuración o sistemas antiguos. Los programadores hacen esto para habilitar el intercambio de datos, configuraciones o incluso llamadas a procedimientos remotos en un formato estructurado y jerárquico que es legible tanto para humanos como para máquinas.

## Cómo hacerlo:

Dart no incluye soporte integrado para el manejo de XML en su biblioteca estándar, lo que hace necesario el uso de paquetes de terceros. Un paquete popular es `xml`. Para usarlo, primero necesitas agregarlo a tu `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Usa la última versión disponible
```

Luego, importa el paquete en tu archivo Dart:

```dart
import 'package:xml/xml.dart' as xml;
```

**Analizando XML:**

Supón que tienes una cadena XML como esta:

```xml
<String name="greeting">Hola, mundo!</String>
```

Puedes analizar y leer el XML de la siguiente manera:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Muestra: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hola, mundo!</String>';
  parseXml(xmlString);
}
```

**Creando Documentos XML:**

Crear un nuevo documento XML es sencillo con el paquete `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hola, mundo!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Salida**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hola, mundo!</greeting>
```

**Consultando y Modificando XML:**

Para encontrar o modificar elementos, puedes usar métodos similares a XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modificando el atributo 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Añadiendo un nuevo elemento hijo
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('¡Adiós!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hola, mundo!</greeting>';
  modifyXml(xmlString);
}
```

**Salida**:

```xml
<greeting name="greeting_modified">
  Hola, mundo!
  <message>¡Adiós!</message>
</greeting>
```

Estos ejemplos demuestran operaciones básicas para trabajar con XML en Dart. Con el paquete `xml`, puedes analizar, crear y manipular documentos XML para adaptarlos a los requisitos de tu aplicación.
