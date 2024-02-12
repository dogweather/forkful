---
title:                "Trabalhando com XML"
aliases:
- /pt/javascript/working-with-xml/
date:                  2024-01-26T04:32:40.891677-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com XML significa analisar, manipular e produzir conteúdo XML usando código. Programadores fazem isso porque o XML é amplamente utilizado para arquivos de configuração, troca de dados e serviços web devido à sua natureza legível por humanos e interpretável por máquinas.

## Como fazer:

Aqui está como analisar XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Usuário</to>
                    <from>Autor</from>
                    <heading>Lembrete</heading>
                    <body>Não me esqueça neste fim de semana!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Saída: Usuário
```

E para produzir XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Usuário';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Saída: <note><to>Usuário</to></note>
```

## Aprofundando

XML é a sigla para eXtensible Markup Language, um formato de dados que existe desde o final dos anos 90. Ele define um conjunto de regras para codificar documentos que tanto humanos quanto máquinas podem ler. Historicamente, o XML ganhou tração pela sua flexibilidade e hierarquia estruturada, tornando-se uma escolha para serviços web, como SOAP, e numerosos arquivos de configuração.

Alternativas ao XML incluem JSON (JavaScript Object Notation), que se tornou popular pelo seu fácil uso com JavaScript e menor peso. YAML é outra alternativa, valorizada por ser amigável aos humanos e uma escolha comum para configuração.

XML é implementado em JavaScript usando as interfaces DOMParser e XMLSerializer. O DOM XML (Modelo de Objeto de Documento) permite navegar e editar documentos XML assim como você faria com HTML. Apesar da ascensão do JSON, entender XML é chave, já que numerosos sistemas legados e indústrias específicas ainda dependem dele para troca de dados.

## Veja Também

- MDN Web Docs (Análise de XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Tutorial DOM XML): https://www.w3schools.com/xml/dom_intro.asp
- "O que é XML?": https://www.w3.org/XML/
