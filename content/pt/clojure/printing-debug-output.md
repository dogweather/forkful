---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
A impressão de saída de depuração é uma forma de expor o estado interno de um programa durante o seu funcionamento. Os programadores fazem isso para entender e encontrar erros e problemas no código.

## Como Fazer:
Aqui estão alguns exemplos de como escrever a saída de depuração em Clojure:

```Clojure
; Imprimir um simples valor de depuração
(println "Valor de depuração:" {:a 1, :b 2})

; Também podemos usar a função prn, que é uma substituição melhor para a impressão de dados estruturados
(prn "Cliente:" {:nome "João", :idade 25})
```

## Mergulho Profundo:
Logo nos primeiros dias de programação, os desenvolvedores perceberam que tinham que ter uma maneira de entender como seu código estava funcionando. Assim, surgiu a prática de imprimir saídas de depuração. Agora, temos métodos mais ricos e ambientes interativos, mas às vezes, quando você está preso, nada supera um bom velho `println`.

Em Clojure, além de `println` e `prn`, você tem outras opções. Uma das mais poderosas é a biblioteca `clojure.tools.logging`. Ela permite controlar a importância do log e direcioná-lo para diferentes destinos.

Debbugging ou a exibição de saída de depuração em Clojure geralmente envolve funções simples como `println` ou `prn` e não exige nenhuma configuração especial. A depuração em Clojure geralmente se beneficia do fato de que é uma linguagem interpretada, o que significa que você pode avaliar partes do código em tempo real.

## Veja Também:
Segue links para aprofundar o conhecimento:

- [Documentação oficial da biblioteca clojure.tools.logging](https://clojure.github.io/tools.logging/)
- [Guia Prático de Clojure](https://practicalli.github.io/clojure/)