---
date: 2024-01-26 04:29:39.292088-07:00
description: "XML \xE9 uma linguagem de marca\xE7\xE3o para codificar documentos de\
  \ forma que sejam leg\xEDveis por humanos e m\xE1quinas. \xC9 chave em servi\xE7\
  os web, arquivos de\u2026"
lastmod: '2024-03-13T22:44:46.220848-06:00'
model: gpt-4-0125-preview
summary: "XML \xE9 uma linguagem de marca\xE7\xE3o para codificar documentos de forma\
  \ que sejam leg\xEDveis por humanos e m\xE1quinas."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
Clojure oferece a biblioteca `clojure.data.xml` para análise (parsing) e emissão de XML. Primeiro, vamos analisar algum XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [conteúdo "<root><foo>bar</foo><foo>baz</foo></root>"
      analisado (xml/parse-str conteúdo)] ; Analisa string XML
  (println analisado))
```
Saída:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Para emitir XML a partir de estruturas Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Saída:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Aprofundamento
XML já tem seu tempo, começando no final dos anos 90 como um subconjunto simplificado do SGML, destinado a dados na web. Seu uso explodiu com tecnologias como SOAP e XHTML, mas recebeu um pouco de concorrência do JSON, que é preferido por sua leveza e simplicidade.

A abordagem de Clojure para XML mantém-se funcional e centrada nos dados, fiel ao ethos da linguagem. `clojure.data.xml` é apenas uma opção; você tem `clojure.xml` para necessidades básicas, e para interoperação com Java, você pode contar com pesos-pesados como JAXB ou DOM4J.

Tenha em mente que o desempenho e uso de memória ao lidar com documentos XML muito grandes pode ser elevado. Analisadores de fluxo como o StAX podem ajudar, mas você precisará recorrer ao Java para eles.

## Veja Também
- [clojure.data.xml no GitHub](https://github.com/clojure/data.xml)
- [API Java para Processamento de XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
