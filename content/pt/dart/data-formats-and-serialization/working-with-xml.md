---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:41.494401-07:00
description: "Trabalhar com XML em Dart envolve analisar, consultar e modificar documentos\
  \ XML, um processo crucial para aplica\xE7\xF5es que interagem com servi\xE7os web,\u2026"
lastmod: '2024-03-13T22:44:46.307968-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com XML em Dart envolve analisar, consultar e modificar documentos\
  \ XML, um processo crucial para aplica\xE7\xF5es que interagem com servi\xE7os web,\u2026"
title: Trabalhando com XML
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com XML em Dart envolve analisar, consultar e modificar documentos XML, um processo crucial para aplicações que interagem com serviços web, arquivos de configuração ou sistemas legados. Os programadores fazem isso para permitir a troca de dados, configurações ou até mesmo chamadas de procedimento remoto num formato estruturado, hierárquico, que é tanto legível por humanos quanto interpretável por máquinas.

## Como:

Dart não inclui suporte embutido para o manuseio de XML em sua biblioteca padrão, sendo necessário o uso de pacotes de terceiros. Um pacote popular é o `xml`. Para usá-lo, primeiro você precisa adicioná-lo ao seu `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Use a versão mais recente disponível
```

Depois, importe o pacote no seu arquivo Dart:

```dart
import 'package:xml/xml.dart' as xml;
```

**Analisando XML:**

Suponha que você tenha uma string XML como esta:

```xml
<String name="greeting">Olá, mundo!</String>
```

Você pode analisar e ler o XML da seguinte forma:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Saídas: greeting
}

void main() {
  final xmlString = '<String name="greeting">Olá, mundo!</String>';
  parseXml(xmlString);
}
```

**Criando Documentos XML:**

Criar um novo documento XML é simples com o pacote `xml`:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Olá, mundo!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Saída**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Olá, mundo!</greeting>
```

**Consultando e Modificando XML:**

Para encontrar ou modificar elementos, você pode usar métodos semelhantes ao XPath:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modificando o atributo 'name'
    greeting.setAttribute('name', 'greeting_modified');
    
    // Adicionando um novo elemento filho
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Adeus!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Olá, mundo!</greeting>';
  modifyXml(xmlString);
}
```

**Saída**:

```xml
<greeting name="greeting_modified">
  Olá, mundo!
  <message>Adeus!</message>
</greeting>
```

Estes exemplos demonstram operações básicas para trabalhar com XML em Dart. Com o pacote `xml`, você pode analisar, criar e manipular documentos XML para atender às necessidades de sua aplicação.
