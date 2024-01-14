---
title:                "Clojure: Usando expressões regulares"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Clojure

Expressões regulares são uma ferramenta poderosa para manipulação de textos e podem ser especialmente úteis no contexto de programação em Clojure. Elas permitem encontrar padrões específicos em um texto e realizar diversas ações, desde validação até substituição. Neste artigo, vamos explorar mais sobre o uso de expressões regulares em Clojure e como elas podem ser úteis para o seu código.

## Como usar expressões regulares em Clojure

Para usar expressões regulares em Clojure, primeiro precisamos importar a biblioteca `java.util.regex` no nosso código. A sintaxe para criação de uma expressão regular é utilizando a função `re-pattern`, que recebe como argumento uma string com o padrão que desejamos encontrar. Por exemplo:

``