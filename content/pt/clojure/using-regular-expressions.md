---
title:    "Clojure: Utilizando expressões regulares"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Clojure?

As expressões regulares são uma ferramenta poderosa para manipulação de texto em qualquer linguagem de programação, e em Clojure não é diferente. Com a utilização de expressões regulares, é possível buscar e substituir padrões de texto de forma mais rápida e eficiente, possibilitando uma maior produtividade e precisão no desenvolvimento de código.

## Como utilizar expressões regulares em Clojure

Para utilizar expressões regulares em Clojure, é necessário importar o pacote de funções `clojure.string`, que possui uma variedade de funções para manipulação de strings, incluindo a função `re-find`, que é responsável por encontrar uma correspondência entre a expressão regular e o texto fornecido.

```
(ns regex-example
  (:require [clojure.string :as string]))

(def text-to-check "abc123def") ; texto a ser verificado
(def regex #"\d+") ; expressão regular para encontrar números

(string/re-find regex text-to-check) ; saída: "123"
```

Após encontrar a correspondência, é possível utilizar a função `re-groups` para obter as capturas específicas da expressão regular.

```
(ns regex-example
  (:require [clojure.string :as string]))

(def text-to-check "abc123def") ; texto a ser verificado
(def regex #"\d") ; expressão regular para encontrar números

(->> (string/re-find regex text-to-check) ; saída: "1"
     (string/re-groups)) ; saída: ("1")
```

Assim, com a combinação dessas funções e outras disponíveis em `clojure.string`, é possível realizar diversas operações com expressões regulares em Clojure.

## Mais informações sobre o uso de expressões regulares em Clojure

Embora as funções de `clojure.string` sejam úteis para operações básicas com expressões regulares, é possível aprofundar ainda mais seu conhecimento através da biblioteca `clojure.repl`, que possui uma série de funções avançadas para trabalhar com expressões regulares em Clojure.

## Veja também

- [Documentação oficial de `clojure.string`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Documentação oficial de `clojure.repl`](https://clojure.github.io/clojure/clojure.repl-api.html)
- [Tutorial de expressões regulares em Clojure](https://www.braveclojure.com/regular-expressions/)