---
title:                "Escrevendo para o erro padrão"
html_title:           "Clojure: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Quando se está escrevendo código em Clojure, muitas vezes é necessário lidar com erros e bugs. Ao invés de simplesmente parar a execução do programa, escrever para o erro padrão (standard error) pode ser uma forma mais eficaz de encontrar e resolver problemas.

## Como Fazer

Ao escrever para o erro padrão em Clojure, é necessário usar a função *println* e passar uma string como argumento. Aqui está um exemplo simples:

```Clojure
(println "Ocorreu um erro.")
```
Isso imprimirá a mensagem "Ocorreu um erro." no erro padrão. Além disso, também é possível usar a função *println* para imprimir informações sobre variáveis e objetos no erro padrão. Por exemplo:

```Clojure
(def nome "João")
(println "O nome é:" nome)
```

Isso imprimirá "O nome é: João" no erro padrão. Usar *println* é uma forma fácil e útil de depurar e encontrar problemas em seu código Clojure.

## Deep Dive

Além da função *println*, também é possível usar a função *prn* para escrever para o erro padrão. A diferença entre as duas é que *prn* imprime os valores no erro padrão sem adicionar uma nova linha ao final. Isso pode ser útil quando se quer imprimir múltiplos valores em uma única linha de erro. Por exemplo:

```Clojure
(def x 1)
(def y 2)
(prn "Valores de x e y:" x y)
```

Isso imprimirá "Valores de x e y: 1 2" no erro padrão, em uma única linha.

## Veja Também

- Documentação oficial sobre a função *println*: https://clojuredocs.org/clojure.core/println
- Documentação oficial sobre a função *prn*: https://clojuredocs.org/clojure.core/prn