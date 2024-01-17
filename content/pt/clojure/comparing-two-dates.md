---
title:                "Comparando duas datas"
html_title:           "Clojure: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Comparar duas datas é uma tarefa comum na programação, utilizada para verificar a relação temporal entre dois eventos ou para analisar mudanças ao longo do tempo. Programadores frequentemente fazem uso dessa funcionalidade para ajudar no desenvolvimento de aplicações relacionadas com datas e tempo.

## Como fazer:
Em Clojure, você pode comparar duas datas utilizando a função `compare`, que retorna `-1` se a primeira data for anterior, `0` se as datas forem iguais e `1` se a primeira data for posterior à segunda. Veja o exemplo abaixo:
```Clojure
(def data1 (java.util.Date. 118, 2, 2))
(def data2 (java.util.Date. 118, 2, 3))
(compare data1 data2)   ; Retorna -1
```

## Profundidade:
Comparar datas tem sido importante na história da computação para garantir a precisão em cálculos de tempo e para evitar problemas relacionados com o bug do milênio. Existem outras abordagens para a comparação de datas, como a utilização de bibliotecas externas ou a criação de funções personalizadas, mas a função `compare` é nativa em Clojure e é uma opção simples e eficiente.

## Veja também:
Documentação oficial de Clojure para a função `compare`: https://clojuredocs.org/clojure.core/compare
Artigo do blog "Effective Programs" sobre comparação de datas em Clojure: https://effective-programming.blogspot.com/2012/07/comparing-dates-in-clojure.html