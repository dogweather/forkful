---
title:                "Clojure: Escrevendo testes"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante para o seu código

Escrever testes é uma parte crucial do processo de desenvolvimento de software. É uma maneira de garantir que o seu código está funcionando corretamente e que qualquer mudança que você fizer não terá um impacto negativo em outras partes do seu programa. Além disso, testes bem escritos ajudam a manter o código organizado e facilitam o processo de depuração.

## Como escrever testes em Clojure

A linguagem de programação Clojure possui uma estrutura de testes integrada, chamada `clojure.test`, que permite escrever testes de maneira eficiente e eficaz. Veja um exemplo de como escrever um teste simples para uma função que retorna o dobro de um número:

```Clojure
(ns testing.core-test
  (:require [clojure.test :refer :all] [testing.core :refer :all]))

(deftest double-test
  (is (= 4 (double 2))))
```
Neste exemplo, criamos um teste chamado `double-test` que verifica se a função `double` retorna o dobro de um número. Em seguida, usamos a função `is` para comparar o resultado da função com o valor esperado.

Para executar este teste, basta rodar o seguinte comando no terminal:

```
lein test
```

Isso irá executar todos os testes presentes no seu projeto e fornecerá um relatório de resultados.

## Aprofundando nos testes em Clojure

Além de simplesmente verificar se uma função retorna o resultado correto, é importante também considerar alguns outros aspectos ao escrever testes em Clojure. Por exemplo, ao testar funções que lidam com diferentes tipos de dados, é importante testar cada tipo de dado separadamente. Além disso, você também pode usar a função `throws` para verificar se uma função lança uma exceção quando esperado.

É recomendável escrever testes para cada função e não apenas para um caso de uso específico. Desta forma, você pode garantir que qualquer alteração feita no seu código não quebre outras partes que dependem dela.

## Veja também

Aqui estão alguns recursos extras para ajudar você a se aprofundar no mundo dos testes em Clojure:

- [Documentação oficial de `clojure.test`](https://clojure.github.io/clojure/clojure.test-api.html)
- [Curso de Clojure: Testes e Debugging (em inglês)](https://www.braveclojure.com/testing/)
- [Como escrever testes de unidade em Clojure (em inglês)](https://medium.com/cljdoc/how-to-write-clojure-unit-tests-5ae2f64ab3da)

Espero que este artigo tenha sido útil para você começar a escrever testes em Clojure. Testes não só ajudam no processo de desenvolvimento, mas também garantem que o seu código seja mais confiável e de alta qualidade. Então, não se esqueça de incluí-los em seu fluxo de trabalho de programação!