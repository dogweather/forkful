---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:30.493145-07:00
description: "Jak to zrobi\u0107: Dart nie zawiera wbudowanego wsparcia dla obs\u0142\
  ugi XML w swojej bibliotece standardowej, co wymusza korzystanie z pakiet\xF3w stron\
  \ trzecich.\u2026"
lastmod: '2024-03-13T22:44:35.119803-06:00'
model: gpt-4-0125-preview
summary: "Dart nie zawiera wbudowanego wsparcia dla obs\u0142ugi XML w swojej bibliotece\
  \ standardowej, co wymusza korzystanie z pakiet\xF3w stron trzecich."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Dart nie zawiera wbudowanego wsparcia dla obsługi XML w swojej bibliotece standardowej, co wymusza korzystanie z pakietów stron trzecich. Jednym z popularnych pakietów jest `xml`. Aby go użyć, najpierw trzeba dodać go do pliku `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Użyj najnowszej dostępnej wersji
```

Następnie zaimportuj pakiet w swoim pliku Dart:

```dart
import 'package:xml/xml.dart' as xml;
```

**Parsowanie XML:**

Załóżmy, że masz ciąg XML tak jak ten:

```xml
<String name="greeting">Hello, world!</String>
```

Możesz przeanalizować i odczytać XML w następujący sposób:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Wypisuje: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**Tworzenie dokumentów XML:**

Tworzenie nowego dokumentu XML jest proste przy użyciu pakietu `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Wyjście**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**Zapytania i modyfikacje XML:**

Aby wyszukać lub zmodyfikować elementy, możesz użyć metod podobnych do XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modyfikowanie atrybutu 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Dodawanie nowego elementu dziecka
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**Wyjście**:

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

Te przykłady demonstrują podstawowe operacje do pracy z XML w Dart. Dzięki pakietowi `xml`, możesz analizować, tworzyć i manipulować dokumentami XML, aby dostosować je do wymagań swojej aplikacji.
