---
title:    "Clojure: Calculando uma data no futuro ou passado"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Porquê

Às vezes, precisamos saber uma data futura ou passada a partir de uma data atual. Isso pode ser útil em muitos casos, como planejar viagens ou negócios, rastrear prazos ou simplesmente por curiosidade.

## Como fazer

Calcular datas em Clojure é bastante simples. Podemos usar a função `inc` ou `dec` para aumentar ou diminuir uma data em um dia.

```Clojure
(inc (java.util.Date.)) ;; data futura
(dec (java.util.Date.)) ;; data passada
```

Podemos também especificar uma quantidade de dias ou outras unidades de tempo para adicionar ou subtrair de uma data. Para isso, usamos a função `+` ou `-` com as unidades de tempo desejadas.

```Clojure
(+ (java.util.Date.) 2 :days)) ;; data futura com 2 dias de adição
(- (java.util.Date.) 5 :months)) ;; data passada com 5 meses de subtração
```

## Mergulho profundo

Além de adicionar ou subtrair dias em uma data, também é possível especificar outras unidades de tempo, como horas, minutos, segundos e até mesmo milissegundos. Podemos usar a função `+` e `-` com essas unidades, assim como fizemos com os dias.

```Clojure
(+ (java.util.Date.) 3 :hours) ;; data futura com 3 horas de adição
(- (java.util.Date.) 30 :minutes) ;; data passada com 30 minutos de subtração 
```

Podemos também usar outras funções, como `days`, `hours`, `minutes` e `seconds`, para obter a diferença entre duas datas em uma determinada unidade de tempo.

```Clojure
(days (java.util.Date.) (org.joda.time.DateTime. "2020-02-29T00:00:00")) ;; diferença em dias entre a data atual e 29 de fevereiro de 2020
(minutes (org.joda.time.LocalDate.) (org.joda.time.DateTime. "2020-07-13T12:00:00")) ;; diferença em minutos entre a data atual e 13 de julho de 2020 às 12:00 
```

## Veja também

- A documentação oficial do Clojure sobre data e hora: https://clojuredocs.org/clojure.java-time
- Mais informações sobre a biblioteca Joda-Time utilizada nos exemplos: http://www.joda.org/joda-time/
- Uma discussão sobre diferentes formas de calcular datas em Clojure: https://stackoverflow.com/questions/25145453/calculating-date-values-in-clojure

O cálculo de datas pode ser muito útil em diversas situações e, com as funções e bibliotecas disponíveis em Clojure, podemos realizar esses cálculos de forma rápida e simples. Esperamos que este artigo tenha sido útil e que você possa aproveitar esses recursos em seus projetos futuros.