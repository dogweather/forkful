---
title:    "Clojure: Comparando duas datas"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que

Comparar datas é uma tarefa comum na programação, seja para validar entradas do usuário, calcular diferenças de tempo ou qualquer outra aplicação relacionada a datas. Neste artigo, exploraremos como comparar duas datas em Clojure de forma simples e eficiente.

## Como Fazer

Para comparar duas datas em Clojure, é necessário primeiro convertê-las para o formato `java.util.Date`. Isso pode ser feito utilizando a função `clj-time.core/parse` do pacote [clj-time](https://github.com/clj-time/clj-time). Em seguida, podemos usar os operadores de comparação `>` (maior que), `<` (menor que), `=` (igual a) e `>=` (maior ou igual a) para comparar as datas.

```Clojure
(ns date-comparison.core
  (:require [clj-time.core :as time]))

;; converte as datas para o formato java.util.Date
(def data-1 (time/parse "01/01/2021" "dd/MM/yyyy"))
(def data-2 (time/parse "31/12/2021" "dd/MM/yyyy"))

;; realiza as comparações
(> data-2 data-1)  ; true
(< data-1 data-2)  ; true
(= data-1 data-2)  ; false
(>= data-2 data-1) ; true
```

Além dos operadores de comparação, também podemos utilizar a função `time/compare` para obter um valor numérico representando a diferença entre as duas datas. Se as datas forem iguais, o valor retornado será `0`. Se a primeira data for maior que a segunda, o valor será `1`. E se a primeira data for menor que a segunda, o valor será `-1`.

```Clojure
;; compara as datas e retorna um número representando a diferença
(time/compare data-1 data-2) ; -1
```

## Mergulho Profundo

Ao comparar datas, é importante considerar também a precisão. Enquanto as funções mencionadas acima são úteis para comparar datas com precisão de dias, para comparações mais precisas (por exemplo, segundos ou milissegundos), é necessário utilizar um tipo de dados diferente, como `java.util.Calendar` ou `java.time.LocalDate` do pacote [java-time](https://clojure.org/reference/java_interop#_java_time_api). No entanto, vale ressaltar que esses tipos ainda podem ser comparados usando os mesmos operadores mencionados anteriormente.

## Veja Também

- [Documentação do pacote clj-time](https://clj-time.github.io/clj-time/)
- [Referência de interoperabilidade com Java](https://clojure.org/reference/java_interop)