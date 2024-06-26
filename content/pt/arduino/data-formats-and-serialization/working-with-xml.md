---
date: 2024-01-26 04:27:23.265478-07:00
description: "Como fazer: Usaremos a biblioteca `XMLWriter` para criar XML e a biblioteca\
  \ `tinyxml2` para analis\xE1-lo. Primeiro, instale as bibliotecas via Gerenciador\u2026"
lastmod: '2024-03-13T22:44:46.862257-06:00'
model: gpt-4-0125-preview
summary: "Usaremos a biblioteca `XMLWriter` para criar XML e a biblioteca `tinyxml2`\
  \ para analis\xE1-lo."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
Usaremos a biblioteca `XMLWriter` para criar XML e a biblioteca `tinyxml2` para analisá-lo. Primeiro, instale as bibliotecas via Gerenciador de Bibliotecas no seu IDE Arduino.

Criando um documento XML:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Usando Serial para saída
  
  xml.header();
  xml.tag("greeting").tag("text").text("Olá, mundo!").close().close();
  xml.flush();
}

void loop() {
}
```

Decodificando uma string XML:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Olá, mundo!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Saída de exemplo:

```
<greeting>
  <text>Olá, mundo!</text>
</greeting>
```

## Aprofundando
XML, ou Extensible Markup Language, é uma linguagem de marcação que define um conjunto de regras para codificar documentos em um formato que é legível tanto por humanos quanto por máquinas. Está presente desde o final dos anos 90 e é usado extensivamente em vários campos, especialmente onde é necessária a troca de dados independente de plataforma. Os recursos limitados de memória do Arduino tornam trabalhar com XML mais desafiador do que em um PC. Por isso, bibliotecas leves são cruciais. Embora o JSON tenha ganhado popularidade para troca de dados devido à sua sintaxe mais simples e menor pegada, o XML ainda é amplamente usado, especialmente quando se lida com sistemas ou aplicações legados que requerem validação de documentos via esquemas. A chave para a implementação de XML no Arduino é a análise em stream, que lê o documento em segmentos para manter o uso de memória baixo.

## Veja Também
- [Documentação da Biblioteca TinyXML-2](https://leethomason.github.io/tinyxml2/)
- [Biblioteca Arduino JSON](https://arduinojson.org/) para uma alternativa ao trabalhar com dados JSON.
- [Tutorial XML do W3Schools](https://www.w3schools.com/xml/) para aprendizado geral sobre XML.
- [Especificação XML do W3C](https://www.w3.org/XML/) para os padrões e recomendações oficiais de XML.
