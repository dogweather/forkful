---
title:                "Arbeiten mit XML"
date:                  2024-03-08T21:57:36.831575-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit XML in Dart beinhaltet das Parsen, Abfragen und Modifizieren von XML-Dokumenten, ein Prozess, der für Applikationen, die mit Webdiensten, Konfigurationsdateien oder Altsystemen interagieren, entscheidend ist. Programmierer nutzen dies, um Datenaustausch, Konfigurationen oder sogar Remote-Prozeduraufrufe in einem strukturierten, hierarchischen Format zu ermöglichen, das sowohl für Menschen lesbar als auch für Maschinen parsbar ist.

## Wie zu:

Dart enthält keine integrierte Unterstützung für die XML-Verarbeitung in seiner Standardbibliothek, was die Verwendung von Drittanbieter-Paketen erforderlich macht. Ein beliebtes Paket ist `xml`. Um es zu verwenden, müssen Sie es zunächst zu Ihrer `pubspec.yaml` hinzufügen:

```yaml
dependencies:
  xml: ^5.0.0 // Verwenden Sie die neueste verfügbare Version
```

Dann importieren Sie das Paket in Ihre Dart-Datei:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML Parsen:**

Angenommen, Sie haben eine XML-Zeichenkette wie diese:

```xml
<String name="greeting">Hallo, Welt!</String>
```

Sie können das XML wie folgt parsen und lesen:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Gibt aus: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hallo, Welt!</String>';
  parseXml(xmlString);
}
```

**XML-Dokumente erstellen:**

Ein neues XML-Dokument zu erstellen ist unkompliziert mit dem `xml` Paket:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hallo');
    builder.text('Hallo, Welt!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Ausgabe**:

```xml
<?xml version="1.0"?>
<greeting name="hallo">Hallo, Welt!</greeting>
```

**XML Abfragen und Modifizieren:**

Um Elemente zu finden oder zu modifizieren, können Sie XPath-ähnliche Methoden verwenden:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modifizieren des 'name' Attributs
    greeting.setAttribute('name', 'greeting_modified');
    
    // Hinzufügen eines neuen Kind-Elements
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Auf Wiedersehen!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hallo">Hallo, Welt!</greeting>';
  modifyXml(xmlString);
}
```

**Ausgabe**:

```xml
<greeting name="greeting_modified">
  Hallo, Welt!
  <message>Auf Wiedersehen!</message>
</greeting>
```

Diese Beispiele demonstrieren grundlegende Operationen für die Arbeit mit XML in Dart. Mit dem `xml` Paket können Sie XML-Dokumente parsen, erstellen und manipulieren, um die Anforderungen Ihrer Applikation zu erfüllen.
