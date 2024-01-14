---
title:                "Clojure: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Saber a data atual pode ser extremamente útil para muitos programas em Clojure. Seja para registrar a data de criação de um arquivo, para calcular a idade de alguém ou até mesmo para exibir a data em um aplicativo, saber como obter e manipular a data pode ser muito útil em suas aplicações.

## Como Fazer

Para obter a data atual em Clojure, usamos a função `java.util.Date`. Esta função retorna a data atual e hora no formato UTC (Tempo Universal Coordenado) em um objeto Date. Veja o exemplo abaixo:

```Clojure
(import '(java.util Date))

(def current-date (Date.)) ; Obtém a data atual
(println current-date) ; Imprime a data atual
```

O código acima irá imprimir a data atual no formato `Sat Dec 04 01:43:42 UTC 2021`. Mas o que acontece se quisermos formatar a data de uma maneira diferente? Podemos usar a biblioteca `clj-time` para nos ajudar com isso. Primeiro precisamos adicionar a dependência `clj-time` em nosso arquivo `project.clj`:

```Clojure
[clj-time "0.15.2"]
```

E então podemos usar a função `clj-time.format/unparse` para formatar a data. Por exemplo, podemos obter a data atual no formato `dd/mm/yyyy`:

```Clojure
(require '[clj-time.format :as time-format])

(def current-date (time-format/unparse "dd/MM/yyyy" (Date.)))
(println current-date) ; Imprime a data atual no formato dd/mm/yyyy
```

Podemos também obter informações específicas da data, como o dia da semana, o mês e o ano, usando as funções disponíveis em `clj-time`.

## Mais Detalhes

Além das funções mencionadas acima, também podemos usar a biblioteca Java `java.text.SimpleDateFormat` para formatar a data de acordo com nossas necessidades. Além disso, é importante lembrar que ao obter a data atual, estamos obtendo a data e hora no fuso horário do sistema. Portanto, é importante considerar isso ao trabalhar com datas em diferentes fusos horários.

## Veja Também

- [Documentação oficial do Clojure para a função `java.util.Date`](https://clojuredocs.org/clojure.core/doto)
- [Documentação oficial do Clojure para a biblioteca `clj-time`](https://github.com/clj-time/clj-time)
- [Documentação oficial da biblioteca Java `java.text.SimpleDateFormat`](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)