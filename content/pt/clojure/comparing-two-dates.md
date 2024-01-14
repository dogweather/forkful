---
title:    "Clojure: Comparando duas datas"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas no Clojure?

Comparar duas datas é uma tarefa bastante comum em muitos programas, especialmente quando se trabalha com dados relacionados a tempo. No Clojure, existem diversas maneiras de comparar datas, cada uma com diferentes vantagens e finalidades. Neste artigo, vamos explorar como comparar duas datas no Clojure e como essa habilidade pode ser útil em seus projetos.

## Como fazer

Para comparar duas datas no Clojure, podemos utilizar a função `comparar` do namespace `clojure.java-time`. Essa função nos permite comparar duas datas e obter um valor de retorno que indica qual data é maior, menor ou igual à outra. Por exemplo:

```Clojure
(require '[clojure.java-time :as time])

(time/compare (time/local-date 2021 08 15) (time/local-date 2021 08 18))
```

Neste caso, a função `compare` irá retornar o valor `-1`, indicando que a primeira data é anterior à segunda. Além disso, também podemos utilizar a função `before?` ou `after?` para verificar se uma data é anterior ou posterior à outra, respectivamente. Veja um exemplo abaixo:

```Clojure
(time/before? (time/local-date 2021 08 15) (time/local-date 2021 08 18))
```

A função irá retornar `true` caso a primeira data seja anterior à segunda e `false` caso contrário.

## Profundidade

Ao comparar datas no Clojure, é importante levar em consideração que existem diferentes precisões de datas, como ano, mês, dia, hora, minuto e segundo. Por isso, é importante garantir que as duas datas estejam na mesma precisão antes de fazer a comparação. Caso contrário, o resultado pode ser imprevisível.

Outro ponto importante é que a comparação leva em consideração a ordem cronológica das datas, então é importante ter isso em mente ao utilizar a função `compare`.

## Veja também

Para mais informações sobre como trabalhar com datas no Clojure, você pode conferir a documentação oficial do namespace `clojure.java-time`. Além disso, o livro "Clojure for the Brave and True" também possui um capítulo dedicado às funcionalidades de datas no Clojure. E, é claro, não deixe de explorar e experimentar por conta própria!