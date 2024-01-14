---
title:                "Clojure: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser extremamente útil em diversos projetos de programação. Imagine que você precisa desenvolver um sistema de gestão de tarefas e precisa definir uma data para uma tarefa ser concluída. Saber como calcular datas futuras ou passadas pode facilitar esse processo.

## Como Fazer

Para calcular uma data no futuro ou passado em Clojure, podemos utilizar a função `clj-time.core/plus` do pacote `clj-time`. Essa função recebe três parâmetros: a data base, um intervalo em dias e um tipo de período (pode ser `:day`, `:month`, `:year`, entre outros).

Vejamos um exemplo de código que calcula a data de 10 dias a partir de hoje:

```
Clojure (require '[clj-time.core :as time])

(def data-hoje (today)) ; define a data de hoje
(def data-futura (plus data-hoje 10 :day)) ; calcula a data de 10 dias no futuro

(time/format data-futura "dd/MM/yyyy") ; imprime a data no formato dd/MM/yyyy
```

Saída: `23/05/2021`

E se quisermos calcular uma data no passado? Basta usar um número negativo no intervalo:

```
Clojure (require '[clj-time.core :as time])

(def data-hoje (today)) ; define a data de hoje
(def data-passada (plus data-hoje -5 :month)) ; calcula a data de 5 meses atrás

(time/format data-passada "dd/MM/yyyy") ; imprime a data no formato dd/MM/yyyy
```

Saída: `09/01/2021`

## Mergulho Profundo

Além da função `plus`, o pacote `clj-time` também oferece outras funções úteis para trabalhar com datas, como `minus` (para subtrair intervalos), `minus-years` (para subtrair anos), `month-of` (para obter o mês de uma data), entre outras.

É importante notar também que todas as funções do pacote requerem que a data seja do tipo `clj-time.core.local-date`, então é necessário converter a data para esse tipo antes de realizar os cálculos.

## Veja Também

- [Documentação oficial do pacote clj-time em Clojure](https://clj-time.github.io/clj-time/)
- [Tutorial para calcular datas no futuro com Clojure](https://medium.com/@speinber/genial-clojure-calculando-fecha-futuras-y-pasadas-7951927165d3)
- [Exemplos de uso da função `clj-time.core/plus`](https://www.programmersought.com/article/16065429463/)