---
title:                "Escrevendo testes"
html_title:           "Clojure: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## O que é e Porquê?

Escrever testes é uma prática comum entre programadores para garantir que o código funciona conforme o esperado. Isso envolve criar códigos que verificam o comportamento de determinadas funções ou módulos, garantindo que não haja erros ou bugs no código.

## Como fazer:

Para escrever testes em Clojure, usamos a biblioteca integrada "clojure.test". Primeiramente, definimos uma função de teste com a palavra-chave "deftest" e um nome descritivo. Dentro dessa função, usamos a macro "is" para testar uma expressão e o valor esperado dela. Por exemplo:

```
(clojure.test/deftest test-soma
  (clojure.test/is (= 10 (+ 6 4)))
)
```

Em seguida, executamos todos os nossos testes usando a função "run-tests" e esperamos que nenhum deles falhe. Se algum teste falhar, receberemos uma mensagem de erro indicando qual teste e qual resultado esperado. 

## Aprofundando:

A escrita de testes é uma prática comum em linguagens de programação funcional, como Clojure, porque é uma forma de garantir que o código esteja livre de bugs e seja fácil de manter. Além disso, testes bem escritos também atuam como documentação para o código, tornando mais fácil para outros programadores entenderem sua funcionalidade.

Existem outras bibliotecas populares para escrever testes em Clojure, como "speclj" e "midje". Cada uma tem suas próprias vantagens e desvantagens, mas todas seguem o mesmo princípio básico de testar o código em níveis granulares.

Outra prática comum em Clojure é usar a técnica de "test-driven development" (TDD), onde os testes são escritos antes mesmo do código de implementação, guiando o processo de desenvolvimento.

## Veja também:

- [Documentação oficial do clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Biblioteca "speclj"](https://github.com/slagyr/speclj)
- [Biblioteca "midje"](https://github.com/marick/Midje)