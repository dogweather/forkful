---
title:    "Clojure: Cálculo de uma data no futuro ou passado"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que Calcular uma Data no Futuro ou Passado?

Um dos motivos principais para calcular uma data no futuro ou passado é para automatizar tarefas e facilitar a vida do programador. Em vez de ter que calcular manualmente uma data, o programa pode fazer isso de forma rápida e precisa.

## Como Calcular uma Data no Futuro ou Passado

Para calcular uma data no futuro ou passado em Clojure, é necessário usar a função `inc` (incremento) ou `dec` (decremento) junto com a função `+` ou `-`, respectivamente. Por exemplo, se quisermos calcular a data que será daqui a 5 dias, podemos escrever o seguinte código:

```Clojure
(println (inc (+ 5 days)))
```

Este código irá imprimir a data de hoje mais 5 dias. Existem várias outras maneiras de manipular datas em Clojure, então é importante explorar mais a documentação e experimentar diferentes abordagens.

Para calcular uma data no passado, podemos usar a função `dec` da seguinte maneira:

```Clojure
(println (dec (- 10 days)))
```

Este código irá imprimir a data de hoje menos 10 dias. É importante observar que, em ambas as situações, a função `days` é usada para identificar qual unidade de tempo estamos manipulando (dias, meses, anos, etc.).

## Mergulho Profundo

Para ter uma compreensão mais profunda de como calcular datas no futuro ou passado em Clojure, é importante entender como as datas são representadas na linguagem. Em Clojure, as datas são representadas como instâncias da classe `java.util.Date` e podem ser manipuladas usando várias funções e bibliotecas.

Uma maneira de tornar o cálculo de datas ainda mais preciso é usando a biblioteca `clj-time`, que fornece funções e macros para manipular datas de forma mais fácil e intuitiva. Vale a pena explorar esta biblioteca e aprender como usá-la de forma eficiente em seus projetos.

## Veja Também

- [Documentação Clojure sobre manipulação de datas](https://clojure.org/guides/date_time)
- [GitHub da biblioteca clj-time](https://github.com/clj-time/clj-time)
- [Tutorial sobre manipulação de datas em Clojure](https://blog.michielborkent.nl/2019/05/06/using-dates-in-clojure.html)