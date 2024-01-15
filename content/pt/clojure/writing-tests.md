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

## Por que

Escrever testes é fundamental para garantir que o código que escrevemos funcione corretamente e continue funcionando conforme fazemos alterações. Além disso, testes bem escritos também servem como documentação para o código, facilitando o entendimento e manutenção no futuro.

## Como fazer

Escrever testes em Clojure é simples e pode ser feito utilizando a library padrão `clojure.test`. Vamos ver um exemplo de teste para uma função que soma dois números:

```Clojure
(ns testes
  (:require [clojure.test :refer :all]))

(defn soma [a b]
  (+ a b))

(deftest testar-soma
  (is (= 4 (soma 2 2)))
  (is (= -2 (soma 5 -7))))

(run-tests)
```

Neste exemplo, estamos definindo o namespace `testes` e importando todas as funções da library `clojure.test`. Em seguida, criamos uma função `soma` que recebe dois argumentos e retorna a soma deles.

Dentro do bloco `deftest`, escrevemos nossos testes utilizando a função `is`, que compara o valor retornado pela função `soma` com o valor esperado utilizando a macro `=`. Por fim, chamamos a função `run-tests` para executar todos os testes definidos.

A saída deste código será:

```
Testing testes

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Isso significa que nossos testes foram executados com sucesso e nenhuma falha foi encontrada. Caso algum teste falhe, a saída será diferente e mostrará exatamente qual teste falhou e qual valor foi retornado.

## Deep Dive

Apesar de nosso exemplo ser simples, existem várias outras funções e macros disponíveis na library `clojure.test` para escrever testes mais complexos e abrangentes. Além disso, existem também outras libraries, como a `midje` e a `speclj`, que oferecem outras funcionalidades e abordagens para testes em Clojure.

É importante lembrar que os testes devem ser escritos de forma que eles continuem funcionando mesmo com alterações no código, ou seja, devem ser resistentes a mudanças. Para isso, é comum utilizar técnicas de mock e stub, que permitem simular o comportamento de funções ou módulos externos durante os testes.

Outro conceito importante é o TDD (Test Driven Development), que consiste em escrever os testes antes de implementar o código. Isso ajuda a pensar no design e nas funcionalidades do código antes de escrevê-lo, além de garantir que as alterações não quebrem o funcionamento do sistema.

## Veja também

- [Documentação da library clojure.test](https://clojuredocs.org/clojure.test)
- [Documentação da library midje](https://github.com/marick/Midje/wiki)
- [Documentação da library speclj](https://github.com/slagyr/speclj)