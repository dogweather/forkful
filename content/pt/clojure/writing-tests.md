---
title:                "Escrevendo testes"
date:                  2024-01-19
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Escrever testes é criar um conjunto de verificações automatizadas para garantir que seu código funcione como esperado. Programadores fazem isso para reduzir bugs, validar comportamentos e economizar tempo a longo prazo.

## Como fazer:
```Clojure
;; Adicionando a biblioteca de testes
(require '[clojure.test :refer :all])

;; Exemplo de um teste simples
(deftest teste-soma
  (testing "Adição de números"
    (is (= 4 (+ 2 2)))))
    
;; Rodando os testes
(run-tests)

;; Saída esperada: 
;; Testing user
;; Ran 1 tests containing 1 assertions.
;; 0 failures, 0 errors.
```

## Mergulho profundo
Os testes têm suas raízes no desenvolvimento de software dos anos 50, mas só ficaram populares com a chegada de métodos ágeis. Alternativas ao `clojure.test` incluem `Midje` e `Expectations`, que oferecem sintaxes e funcionalidades diferentes. A implementação de testes em Clojure é simples: use `deftest` para definir um teste e `is` para afirmar comportamentos esperados.

## Veja também
- Documentação oficial `clojure.test`: https://clojure.github.io/clojure/clojure.test-api.html
- Guia para testes em Clojure com `Midje`: https://github.com/marick/Midje
- Uma olhada mais aprofundada em testes com `Expectations`: https://github.com/clojure-expectations/clojure-test
