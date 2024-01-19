---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings significa unir duas ou mais strings em uma só. Programadores usam isso para criar mensagens personalizadas, formatar saídas de dados e para o processo de manipulação de strings.

## Como fazer:

Em Clojure, a função `str` é principalmente usada para concatenar strings. Aqui está um exemplo:

```Clojure
(str "Olá, " "mundo!") ;; Reúne "Olá, " e "mundo!" em uma única string.
```

A saída será:

`Olá, mundo!`

Você pode concatenar quantas strings quiser.

```Clojure
(str "Eu " "estou " "programando " "em " "Clojure.") ;; Reúne todas as strings em uma única string.
```

A saída será:

`Eu estou programando em Clojure.`

## Aprofundando

A função `str` foi incluída no Clojure desde a primeira versão, proporcionando uma forma simples e eficaz de concatenar strings. Há também alternativas como `format` ou `clojure.string/join`, que são usadas para formatos mais complexos ou quando há uma lista de strings que precisam ser unidas.

Os detalhes de implementação subjacentes da concatenação de strings diferem entre as linguagens de programação. Em linguagens como Java e Clojure, que são de tipo imutável, a concatenação resulta em novas strings alocadas, enquanto outras linguagens podem permitir a modificação no lugar.

## Veja Também

Confira a documentação oficial do Clojure sobre funções de string para um entendimento mais aprofundado: [Clojure String API](https://clojure.github.io/clojure/clojure.string-api.html)

Este post do blog explica mais sobre a manipulação de strings em Clojure: [Manipulating Strings in Clojure](http://blog.jayfields.com/2010/07/clojure-text-manipulation.html)