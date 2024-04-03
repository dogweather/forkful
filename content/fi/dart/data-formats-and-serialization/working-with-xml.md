---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:02.628473-07:00
description: "XML:n k\xE4sittely Dartissa sis\xE4lt\xE4\xE4 XML-dokumenttien j\xE4\
  sennyksen, kyselyn ja muokkauksen, joka on olennainen prosessi sovelluksille, jotka\
  \ ovat\u2026"
lastmod: '2024-03-13T22:44:56.298056-06:00'
model: gpt-4-0125-preview
summary: "XML:n k\xE4sittely Dartissa sis\xE4lt\xE4\xE4 XML-dokumenttien j\xE4sennyksen,\
  \ kyselyn ja muokkauksen, joka on olennainen prosessi sovelluksille, jotka ovat\
  \ vuorovaikutuksessa verkkopalveluiden, konfiguraatiotiedostojen tai perint\xF6\
  j\xE4rjestelmien kanssa."
title: "Ty\xF6skentely XML:n kanssa"
weight: 40
---

## Mikä & Miksi?

XML:n käsittely Dartissa sisältää XML-dokumenttien jäsennyksen, kyselyn ja muokkauksen, joka on olennainen prosessi sovelluksille, jotka ovat vuorovaikutuksessa verkkopalveluiden, konfiguraatiotiedostojen tai perintöjärjestelmien kanssa. Ohjelmoijat tekevät tämän mahdollistaakseen tietojenvaihdon, konfiguraatiot tai jopa etämenettelykutsut rakenteellisessa, hierarkisessa muodossa, joka on sekä ihmisen luettavissa että koneellisesti jäsentävissä.

## Kuinka tehdä:

Dart ei sisällä sisäänrakennettua tukea XML:n käsittelyyn sen standardikirjastossa, mikä edellyttää kolmansien osapuolten pakettien käyttöä. Yksi suosittu paketti on `xml`. Käyttääksesi sitä, sinun on ensin lisättävä se `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  xml: ^5.0.0 // Käytä saatavilla olevaa uusinta versiota
```

Tuo sitten paketti Dart-tiedostoosi:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML:n jäsennys:**

Oletetaan, että sinulla on XML-merkkijono kuten tämä:

```xml
<String name="greeting">Hei, maailma!</String>
```

Voit jäsentää ja lukea XML:n seuraavasti:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Tulostaa: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hei, maailma!</String>';
  parseXml(xmlString);
}
```

**XML-dokumenttien luominen:**

Uuden XML-dokumentin luominen on suoraviivaista `xml`-paketilla:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hei, maailma!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Tuloste**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hei, maailma!</greeting>
```

**XML:n tiedustelu ja muokkaus:**

Elementtien löytämiseen tai muokkaamiseen voit käyttää XPathin kaltaisia menetelmiä:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Muokataan 'name'-attribuuttia
    greeting.setAttribute('name', 'greeting_modified');
    
    // Lisätään uusi lapsielementti
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Näkemiin!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hei, maailma!</greeting>';
  modifyXml(xmlString);
}
```

**Tuloste**:

```xml
<greeting name="greeting_modified">
  Hei, maailma!
  <message>Näkemiin!</message>
</greeting>
```

Nämä esimerkit osoittavat perustoiminnot XML:n käsittelyyn Dartissa. `xml`-paketin avulla voit jäsentää, luoda ja manipuloida XML-dokumentteja sovelluksesi vaatimusten mukaisesti.
