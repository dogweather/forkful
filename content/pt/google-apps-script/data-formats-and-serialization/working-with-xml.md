---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:36.230390-07:00
description: "Trabalhar com XML no Google Apps Script permite que programadores fa\xE7\
  am o parse, manipulem e gerem dados XML, essencial para servi\xE7os web e configura\xE7\
  \xF5es.\u2026"
lastmod: '2024-03-13T22:44:46.135765-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com XML no Google Apps Script permite que programadores fa\xE7\
  am o parse, manipulem e gerem dados XML, essencial para servi\xE7os web e configura\xE7\
  \xF5es.\u2026"
title: Trabalhando com XML
---

{{< edit_this_page >}}

## O Que & Por Quê?

Trabalhar com XML no Google Apps Script permite que programadores façam o parse, manipulem e gerem dados XML, essencial para serviços web e configurações. Programadores adotam essa abordagem para integrar com sistemas legados, realizar web scraping ou comunicar-se com numerosas APIs que ainda dependem do XML em vez do JSON para a troca de dados.

## Como:

O Google Apps Script fornece o `XmlService` para trabalhar com dados XML. Abaixo, demonstramos como fazer o parse de uma string XML, modificar seu conteúdo e gerar uma nova string XML.

Fazendo o parse de uma string XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Registra: Hello
}
```

Para modificar o XML, você pode querer adicionar um novo elemento filho:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Registra a nova string XML com o elemento filho adicionado
}
```

Gerando string XML do zero:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Saídas: <root><child>Hello World</child></root>
}
```

## Aprofundamento

Historicamente, XML (Linguagem de Marcação Extensível) foi o padrão de facto para troca de dados antes de o JSON emergir como uma alternativa leve. A sintaxe verbosa do XML e seu modelo de parse rigoroso forneceram um formato de dados robusto, embora volumoso. No Google Apps Script, a API `XmlService` encapsula a criação, o parse e a manipulação de dados XML, reconhecendo sua importância contínua em vários sistemas legados e empresariais, serviços web SOAP e arquivos de configuração para aplicações.

Apesar da prevalência do JSON no desenvolvimento web moderno por sua simplicidade e facilidade de uso com JavaScript, o XML permanece relevante em áreas onde a validação de documentos e hierarquias estruturadas são cruciais. No entanto, para novos projetos, especialmente aqueles voltados para APIs web, o JSON muitas vezes é a escolha mais prática devido à sua natureza leve e integração perfeita com o JavaScript.

Compreender o XML e seu manuseio no Google Apps Script é primordial para desenvolvedores trabalhando em ambientes onde a integração com sistemas mais antigos ou APIs empresariais específicas é necessária. No entanto, ao iniciar novos projetos ou quando a flexibilidade é chave, é aconselhável avaliar a necessidade do XML em detrimento de alternativas como o JSON.
