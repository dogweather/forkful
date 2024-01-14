---
title:                "Clojure: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Clojure?

Testes são fundamentais para garantir a qualidade e a funcionalidade do seu código em Clojure. Eles permitem que você verifique se o seu código está produzindo os resultados esperados e ajuda a detectar erros antes que eles se tornem um problema maior.

## Como escrever testes em Clojure

Para escrever testes em Clojure, é necessário utilizar a biblioteca de testes integrada, chamada clojure.test. Vamos ver um exemplo de como testar uma função simples que soma dois números:

```Clojure
(ns meuprojeto.teste
  (:require [clojure.test :refer :all]))

(defn soma [x y]
  (+ x y))

(deftest teste-soma
  (testing "Teste simples de adição"
    (is (= 10 (soma 5 5)))))

(run-tests)
```

Neste exemplo, estamos importando a biblioteca de testes e criando uma função de soma simples. Em seguida, criamos um teste utilizando a função `deftest` e dentro dela utilizamos a função `is` para verificar se o resultado da soma é igual a 10. Por fim, utilizamos a função `run-tests` para executar o teste.

Ao rodar este teste, esperamos que o resultado seja positivo, já que 5 + 5 é igual a 10. Caso haja algum erro no código da função `soma`, o teste irá falhar e nos informar o que deu errado. Com isso, podemos corrigir o código e garantir que ele esteja funcionando corretamente.

## Aprofundando nos testes em Clojure

Além da função `is`, existem outras funções úteis para escrever testes em Clojure, como `are` e `are-not` para verificar múltiplos casos de teste, `throws?` para testar se uma função está lançando uma exceção e `every` para verificar se uma determinada condição é verdadeira para todos os elementos de uma coleção.

Além disso, é possível utilizar a macro `testing` para criar testes mais complexos e utilizar descrições mais detalhadas para cada caso de teste. Também é importante ter em mente que os testes devem ser escritos antes do código e sempre devem ser mantidos atualizados conforme o código é alterado.

## Veja também

- [Documentação oficial da biblioteca clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Tutorial de testes em Clojure da plataforma ClojureBridge](https://github.com/ClojureBridge/curriculum/tree/master/testing)
- [Tutorial de testes em Clojure do site Clojure for the Brave and True](https://www.braveclojure.com/testing/)

Esperamos que este artigo tenha sido útil para entender a importância e como escrever testes em Clojure. Utilize-os em seus projetos para ter mais segurança e confiabilidade no seu código. Happy coding!