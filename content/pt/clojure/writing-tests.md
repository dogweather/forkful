---
title:    "Clojure: Escrevendo testes"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# Por que escrever testes é importante para programadores Clojure

Escrever testes é uma prática essencial para qualquer programador, e isso não é diferente para aqueles que trabalham com a linguagem Clojure. Os testes podem ajudá-lo a encontrar e corrigir erros em seu código, garantindo assim que seu programa funcione corretamente e de forma consistente.

## Como escrever testes em Clojure

Para escrever testes eficazes em Clojure, é importante seguir algumas boas práticas. Primeiro, é importante ter um bom entendimento da função que deseja testar e dos possíveis resultados esperados. Em seguida, você pode usar a biblioteca de testes padrão do Clojure, chamada clojure.test, para escrever seus testes.

Vamos ver um exemplo de teste simples usando a função de soma:

```Clojure
(deftest test-sum
  (testing "Soma de dois números inteiros"
    (is (= (+ 2 3) 5))))

```

Nesse exemplo, estamos criando um teste chamado "test-sum" para a função de soma. Em seguida, usamos a função "testing" para descrever o que estamos testando, seguido pela função "is" para comparar o resultado esperado com o resultado real. Se os resultados forem iguais, o teste será aprovado!

## Aprofundando os conceitos de escrita de testes em Clojure

Além dos testes básicos, existem outras técnicas que podem ser usadas para escrever testes mais abrangentes e eficazes em Clojure. Por exemplo, você pode usar a função "are" para testar vários casos de teste para a mesma função. Além disso, é possível usar a função "with-test" para executar algumas etapas adicionais antes e depois de cada teste.

Além disso, é importante lembrar que em Clojure, os testes são escritos como código regular, o que significa que você pode usar qualquer recurso da linguagem para criar testes mais dinâmicos e abrangentes.

# Veja também

- Documentação da biblioteca de testes do Clojure: https://clojuredocs.org/clojure.test
- Artigo sobre as melhores práticas de testes em Clojure: https://www.metosin.fi/blog/Best-Practices-for-Testing-Clojure/
- Vídeo sobre como escrever testes em Clojure: https://www.youtube.com/watch?v=ktZxmX4YNYc