---
title:    "Clojure: Escrevendo testes"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que escrever testes em Clojure?

Escrever testes é uma prática importante em qualquer linguagem de programação, e no Clojure não é diferente. Os testes garantem que o código esteja funcionando corretamente e nos ajudam a identificar problemas antes que eles se tornem bugs no código em produção. Além disso, escrever testes nos ajuda a ter um entendimento mais profundo do código e torna o processo de manutenção mais fácil.

## Como escrever testes em Clojure

Para escrever testes em Clojure, podemos usar a biblioteca [clojure.test] (https://clojure.github.io/clojure/clojure.test-api.html). Ela nos fornece funções para definir testes e verificar as saídas esperadas. Veja um exemplo abaixo:

```Clojure
(use 'clojure.test)

(deftest test-soma
  (is (= (+ 2 3) 5)))
```

No código acima, usamos a função `deftest` para definir um novo teste chamado `test-soma`. Dentro do teste, usamos a função `is` para verificar se a soma de 2 e 3 é igual a 5, usando o operador `=`. Podemos executar esse teste com a função `run-tests` e devemos ver uma saída como esta:

```
Testing user
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Se tivéssemos uma saída diferente da esperada, o teste falharia e receberíamos uma mensagem de erro. Podemos ter quantos testes quisermos dentro de um arquivo de teste, basta adicioná-los com a função `deftest` e executar todos eles com `run-tests`.

Outra prática importante em testes é o uso de mocks, que são objetos simulados para testar comportamentos específicos. Podemos usar a biblioteca [clojure.test.mock] (https://github.com/clj-commons/clojure-test-mock) para isso. Veja um exemplo abaixo:

```Clojure
(use 'clojure.test)
(require '[clojure.test.mock :as mock])

(defn sum [a b] (+ a b))

(deftest test-mock
  (mock/with-redefs [sum (fn [_ _] 10)]
    (is (= (sum 2 3) 10))))
```

No código acima, usamos a função `with-redefs` do `clojure.test.mock` para criar um mock da função `sum`, substituindo o seu comportamento para sempre retornar o valor 10. Assim, podemos garantir que a função `sum` sempre se comportará dessa maneira, independentemente do seu código real.

## Guia profundo para escrever testes em Clojure

Ao escrever testes em Clojure, é importante ter em mente o seguinte:

- Teste todas as funções e suas diferentes condições de entrada e saída.
- Use nomes descritivos para os testes, o que torna mais fácil entender o que está sendo testado.
- Escreva testes simples e independentes uns dos outros, para facilitar a manutenção.

Além disso, é recomendável que você use TDD (Test Driven Development) ao escrever código em Clojure. Isso significa escrever os testes primeiro e, em seguida, escrever o código que os faz passar. Isso nos ajuda a ter um código mais seguro e mais fácil de manter.

## Veja também
- [clojure.test] (https://clojure.github.io/clojure/clojure.test-api.html)
- [clojure.test.mock] (https://github.com/clj-commons/clojure-test-mock)
- [The Clojure Test Style Guide] (https://github.com/lambdaisland/clojure-test-style-guide)