---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Clojure: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Calcular uma data no futuro ou passado refere-se ao processo de encontrar uma data que é um número específico de dias, semanas, meses ou anos ante ou após uma data específica. Programadores fazem isso frequentemente para lidar com prazos, agendamentos de eventos ou rastrear períodos de tempo.

## Como fazer:

Vamos usar a biblioteca `clj-time` para esta tarefa. Primeiro, temos de adicionar isto ao nosso project.clj.
```Clojure
:dependencies [[clj-time "0.15.2"]]
```
Agora vamos calcular uma data no futuro.
```Clojure
(use 'clj-time.core)
(use 'clj-time.coerce)

(let [hoje (to-local-date (today))
      daqui-dez-dias (plus hoje (days 10))]
  (to-string daqui-dez-dias))
```
A saída seria uma String da data de dez dias a partir de hoje.

## Mergulho Profundo:

A Biblioteca `clj-time` tem uma interface simples e fluida que faz com que o cálculo de datas seja fácil. Esta biblioteca é na verdade um envoltório fino em torno da biblioteca Joda-Time em JDK, que é uma biblioteca muito capaz, mas tem uma API mais complicada.

Alternativas para `clj-time` incluem as APIs incorporadas de tempo e data do JDK, bem como bibliotecas independentes como `java.time`.

Os detalhes da implementação dos cálculos das datas são baseados na matemática simples, mas é importante estar atento às complexidades do calendário humano, como anos bissextos, fusos horários e a transição para o horário de verão.

## Veja Também:

-  Documentação `clj-time`: https://github.com/clj-time/clj-time
-  Documentação `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
-  Artigo interessante sobre o cálculo de datas: https://codeblog.jonskeet.uk/2017/04/23/all-about-java-util-date/