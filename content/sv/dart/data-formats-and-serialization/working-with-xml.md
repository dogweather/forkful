---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:29.983274-07:00
description: "Hur man g\xF6r: Dart inkluderar inte inbyggt st\xF6d f\xF6r XML-hantering\
  \ i sitt standardbibliotek, vilket kr\xE4ver anv\xE4ndning av tredjepartspaket.\
  \ Ett popul\xE4rt\u2026"
lastmod: '2024-03-13T22:44:37.638469-06:00'
model: gpt-4-0125-preview
summary: "Dart inkluderar inte inbyggt st\xF6d f\xF6r XML-hantering i sitt standardbibliotek,\
  \ vilket kr\xE4ver anv\xE4ndning av tredjepartspaket."
title: Arbeta med XML
weight: 40
---

## Hur man gör:
Dart inkluderar inte inbyggt stöd för XML-hantering i sitt standardbibliotek, vilket kräver användning av tredjepartspaket. Ett populärt paket är `xml`. För att använda det behöver du först lägga till det i din `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Använd den senaste tillgängliga versionen
```

Importera sedan paketet i din Dart-fil:

```dart
import 'package:xml/xml.dart' as xml;
```

**Analysera XML:**

Antag att du har en XML-sträng som denna:

```xml
<String name="greeting">Hej, världen!</String>
```

Du kan analysera och läsa XML på följande sätt:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Utskrift: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hej, världen!</String>';
  parseXml(xmlString);
}
```

**Skapa XML-dokument:**

Det är enkelt att skapa ett nytt XML-dokument med `xml`-paketet:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hej, världen!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Utskrift**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hej, världen!</greeting>
```

**Förfråga och Modifiera XML:**

För att hitta eller ändra element kan du använda XPath-liknande metoder:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modifiera 'name'-attributet
    greeting.setAttribute('name', 'greeting_modified');
    
    // Lägga till ett nytt barn-element
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Hejdå!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hej, världen!</greeting>';
  modifyXml(xmlString);
}
```

**Utskrift**:

```xml
<greeting name="greeting_modified">
  Hej, världen!
  <message>Hejdå!</message>
</greeting>
```

Dessa exempel demonstrerar grundläggande operationer för att arbeta med XML i Dart. Med `xml`-paketet kan du analysera, skapa och manipulera XML-dokument för att uppfylla dina applikationskrav.
