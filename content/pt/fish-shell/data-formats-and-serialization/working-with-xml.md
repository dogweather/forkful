---
date: 2024-01-26 04:30:45.570448-07:00
description: "Trabalhar com XML significa manipular dados em um formato estruturado\
  \ e pervasivo usado em configura\xE7\xF5es, mensagens e mais. Programadores manipulam\
  \ o XML\u2026"
lastmod: '2024-03-13T22:44:47.029936-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com XML significa manipular dados em um formato estruturado e\
  \ pervasivo usado em configura\xE7\xF5es, mensagens e mais."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
O Fish não possui análise de XML incorporada, então você vai depender de ferramentas externas como o `xmllint` ou `xmlstarlet`. Aqui está um trecho para ler valores:

```fish
# Analisar XML usando xmlstarlet
echo '<root><element>Olá Mundo</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Saída:
```
Olá Mundo
```

Para editar XML, use isto:

```fish
# Editar elemento XML usando xmlstarlet
echo '<root><element>Valor Antigo</element></root>' | xmlstarlet ed -u "/root/element" -v 'Novo Valor'
```

Saída:
```xml
<?xml version="1.0"?>
<root>
  <element>Novo Valor</element>
</root>
```

## Aprofundamento:
O XML existe desde o final dos anos 90, criado para ser legível e amigável para máquinas. Enquanto o JSON usurpou parte da popularidade do XML devido à sua simplicidade, o XML permanece enraizado onde a validação de documentos e espaços de nomes são chave.

Alternativas? Claro — JSON, YAML, ou até mesmo formatos binários como Protocol Buffers para aqueles aplicativos intensivos em desempenho. Mas o esquema do XML e XSLT (para transformações de XML) podem ser decisivos para cenários complexos onde a robustez importa.

Por baixo do capô, ferramentas como o `xmlstarlet` encapsulam bibliotecas poderosas como o libxml2, oferecendo XPath e XQuery para ajustes finos em XML. Estas não são apenas ferramentas de XML, mas portais para manipulação do DOM, como você aplicaria conceitos similares em qualquer linguagem que toque em XML.

## Veja Também:
- [Documentação do xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Documentação do Fish](https://fishshell.com/docs/current/index.html)
- [Funções e Operadores do XPath e XQuery](https://www.w3.org/TR/xpath-functions/)
