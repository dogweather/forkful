---
title:                "Apagando caracteres que correspondem a um padrão."
html_title:           "Clojure: Apagando caracteres que correspondem a um padrão."
simple_title:         "Apagando caracteres que correspondem a um padrão."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, em nossos programas em Clojure, precisamos manipular strings de forma mais específica, removendo caracteres que correspondem a um determinado padrão. Isso pode ser útil em casos como filtrar dados ou formatar uma string de acordo com um determinado formato.

## Como fazer

Podemos usar a função `clojure.string/replace` para substituir esses caracteres por uma string vazia, efetivamente removendo-os. Por exemplo, se quisermos remover todos os dígitos de uma string, podemos usar a expressão regular `#"[0-9]"` como padrão e passá-la como argumento para a função `replace`. Veja um exemplo:

```Clojure
(require '[clojure.string :as str])

(str/replace "abc123xyz" #"[0-9]" "")
;; resultado: "abcxyz"
```

Podemos combinar esse método com outras funções de manipulação de strings para obter resultados mais complexos. Por exemplo, se quisermos remover todos os caracteres não alfabéticos de uma string, podemos primeiro usar a função `str/replace` para substituir todos os dígitos por uma string vazia e depois combinar isso com a função `str/reduce` para remover todos os caracteres especiais. Veja um exemplo:

```Clojure
(str/reduce (fn [acc x] (if (Character/isLetter x) (str acc x) acc)) (str/replace "abc123.xyz" #"[0-9]" ""))
;; resultado: "abcxyz"
```

## Profundando um pouco mais

A função `replace` utiliza expressões regulares para encontrar padrões em uma string e substituí-los por outra. Essa flexibilidade nos permite realizar operações mais complexas e específicas de acordo com nossas necessidades. Além disso, as expressões regulares são uma ferramenta poderosa para trabalhar com strings em Clojure e vale a pena aprender mais sobre elas e suas funcionalidades.

## Veja também

- [Documentação oficial do Clojure](https://clojure.org/)
- [Tutorial sobre expressões regulares em Clojure](https://www.braveclojure.com/regular-expressions/)
- [Exemplos de uso da função `replace`](https://clojuredocs.org/clojure.string/replace)