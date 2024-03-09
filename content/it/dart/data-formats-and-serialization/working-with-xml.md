---
title:                "Lavorare con XML"
date:                  2024-03-08T21:57:28.788171-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con XML in Dart coinvolge l'analisi, l'interrogazione e la modifica dei documenti XML, un processo fondamentale per le applicazioni che interagiscono con i servizi web, file di configurazione o sistemi legacy. I programmatori fanno ciò per abilitare lo scambio di dati, configurazioni o persino chiamate di procedura remote in un formato strutturato, gerarchico che sia leggibile dall'uomo e analizzabile dalla macchina.

## Come fare:

Dart non include il supporto incorporato per la gestione dell'XML nella sua libreria standard, rendendo necessario l'uso di pacchetti di terze parti. Un pacchetto popolare è `xml`. Per usarlo, devi prima aggiungerlo al tuo `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Usa l'ultima versione disponibile
```

Poi, importa il pacchetto nel tuo file Dart:

```dart
import 'package:xml/xml.dart' as xml;
```

**Analizzare XML:**

Supponiamo che tu abbia una stringa XML come questa:

```xml
<String name="greeting">Ciao, mondo!</String>
```

Puoi analizzare e leggere l'XML come segue:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Stampa: greeting
}

void main() {
  final xmlString = '<String name="greeting">Ciao, mondo!</String>';
  parseXml(xmlString);
}
```

**Creare Documenti XML:**

Creare un nuovo documento XML è semplice con il pacchetto `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Ciao, mondo!');
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
<greeting name="hello">Ciao, mondo!</greeting>
```

**Interrogare e Modificare XML:**

Per trovare o modificare elementi, puoi usare metodi simili a XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modifica dell'attributo 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Aggiunta di un nuovo elemento figlio
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Arrivederci!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Ciao, mondo!</greeting>';
  modifyXml(xmlString);
}
```

**Output**:

```xml
<greeting name="greeting_modified">
  Ciao, mondo!
  <message>Arrivederci!</message>
</greeting>
```

Questi esempi dimostrano le operazioni di base per lavorare con XML in Dart. Con il pacchetto `xml`, puoi analizzare, creare e manipolare documenti XML per soddisfare i requisiti della tua applicazione.
