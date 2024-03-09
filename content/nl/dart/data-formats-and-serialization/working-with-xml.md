---
title:                "Werken met XML"
date:                  2024-03-08T21:57:48.335909-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in Dart omvat het parsen, bevragen en wijzigen van XML-documenten, een proces dat cruciaal is voor applicaties die interageren met webservices, configuratiebestanden of legacy-systemen. Programmeurs doen dit om gegevensuitwisseling, configuraties of zelfs externe procedure-aanroepen mogelijk te maken in een gestructureerd, hiërarchisch formaat dat zowel menselijk leesbaar als door machines te ontleden is.

## Hoe te:

Dart bevat geen ingebouwde ondersteuning voor XML-verwerking in zijn standaardbibliotheek, wat het noodzakelijk maakt om gebruik te maken van pakketten van derden. Een populair pakket is `xml`. Om het te gebruiken, moet je het eerst toevoegen aan je `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Gebruik de laatste beschikbare versie
```

Importeer vervolgens het pakket in je Dart-bestand:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML Parsen:**

Stel dat je een XML-reeks zoals deze hebt:

```xml
<String name="greeting">Hallo, wereld!</String>
```

Je kunt de XML als volgt parsen en lezen:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Outputs: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hallo, wereld!</String>';
  parseXml(xmlString);
}
```

**XML-documenten maken:**

Een nieuw XML-document maken is eenvoudig met het `xml` pakket:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hallo');
    builder.text('Hallo, wereld!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Output**:

```xml
<?xml version="1.0"?>
<greeting name="hallo">Hallo, wereld!</greeting>
```

**XML Bevragen en Wijzigen:**

Om elementen te vinden of te wijzigen, kun je XPath-achtige methoden gebruiken:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Het 'name' attribuut wijzigen
    greeting.setAttribute('name', 'greeting_modified');
    
    // Een nieuw kind-element toevoegen
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Tot ziens!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hallo">Hallo, wereld!</greeting>';
  modifyXml(xmlString);
}
```

**Output**:

```xml
<greeting name="greeting_modified">
  Hallo, wereld!
  <message>Tot ziens!</message>
</greeting>
```

Deze voorbeelden demonstreren basisbewerkingen voor het werken met XML in Dart. Met het `xml` pakket kun je XML-documenten parsen, maken en manipuleren om te voldoen aan de vereisten van je applicatie.
