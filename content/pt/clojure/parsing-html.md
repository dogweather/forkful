---
title:                "Clojure: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Algumas tarefas de programação exigem a extração de informações específicas de páginas da web. Nesses casos, é necessário analisar o código HTML dessas páginas e extrair os dados desejados. A linguagem Clojure oferece ferramentas poderosas para esse fim, tornando o processo de parsing de HTML mais rápido e eficiente.

## Como Fazer

Para analisar HTML em Clojure, é necessário instalar a biblioteca Clojure HTML-XML Parsers. Para isso, podemos adicionar a dependência ao nosso projeto no arquivo "project.clj":

```
Clojure [org.clojure/html-xml-parsers "0.10.0"]
```

Agora, vamos importar o namespace da biblioteca para nosso código e iniciar o parsing de HTML:

```
Clojure (ns meu-projeto
  (:require [net.cgrand.xml-html :as xml]))
  
(defn analisar-html [url]
  (with-open [reader (java.io.StringReader. (slurp url))]
    (xml/xml-seq reader)))
```

Neste exemplo, estamos criando uma função que receberá a URL da página que queremos analisar. A função irá ler o conteúdo da página, convertê-lo em uma string e, em seguida, utilizar a função "xml-seq" para obter um sequenciador dos elementos HTML da página.

Podemos, então, usar funções de Clojure, como "map" ou "filter", para acessar as informações que desejamos do sequenciador retornado. Por exemplo, se quisermos obter o texto dentro de todas as tags "p", podemos fazer o seguinte:

```
Clojure (map :content (filter #(= (:tag %) :p) (analisar-html "https://www.example.com")))
```

Isso irá retornar uma lista com o texto de todos os parágrafos da página.

## Deep Dive

Além da função "xml-seq", a biblioteca Clojure HTML-XML Parsers oferece outras funções úteis para ajudar no parsing de HTML. Algumas delas são:

- "xml/parse" - Para analisar uma string de código HTML e retornar um mapa com os elementos do documento.
- "xml/attr" - Para acessar atributos de um elemento específico.
- "xml/children" - Para obter os filhos de um elemento.
- "xml/select" - Para encontrar elementos específicos com base em seletores CSS.

Com essas ferramentas, é possível realizar uma análise mais precisa e detalhada do HTML de uma página.

## Veja Também

- [Clojure HTML-XML Parsers - Documentação](https://github.com/cgrand/html-xml-utils)
- [10 Common HTML Tag Parsing Mistakes in Clojure](https://kylecordes.com/2017/clojure-html-tag-parsing-mistakes)
- [Tutorial de Clojure - Parsing HTML](https://clojure.org/guides/learn/syntax#_parsing_html)