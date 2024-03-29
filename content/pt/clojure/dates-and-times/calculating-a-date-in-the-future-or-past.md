---
date: 2024-01-20 17:28:32.499239-07:00
description: "Calcular uma data no futuro ou passado consiste em alterar uma data\
  \ base por um certo per\xEDodo de tempo. Programadores realizam essa tarefa para\
  \ agendar\u2026"
lastmod: '2024-03-13T22:44:46.209948-06:00'
model: gpt-4-1106-preview
summary: "Calcular uma data no futuro ou passado consiste em alterar uma data base\
  \ por um certo per\xEDodo de tempo. Programadores realizam essa tarefa para agendar\u2026"
title: Calculando uma data no futuro ou passado
---

{{< edit_this_page >}}

## O quê & Por quê?
Calcular uma data no futuro ou passado consiste em alterar uma data base por um certo período de tempo. Programadores realizam essa tarefa para agendar eventos, calcular prazos ou até verificar a validade de certos dados.

## Como fazer:
```Clojure
;; Adicionando dias a uma data
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.format :as f])

;; Define a data atual
(def today (t/now))

;; Adiciona 5 dias a partir de hoje
(def future-date (t/plus-days today 5))

;; Formatar e imprimir a data futura
(println (f/unparse (f/formatter "dd-MM-yyyy") future-date))
```
Saída Exemplo:
```
"22-03-2023"
```

```Clojure
;; Subtraindo dias de uma data
(def past-date (t/minus-days today 5))

;; Formatar e imprimir a data passada
(println (f/unparse (f/formatter "dd-MM-yyyy") past-date))
```
Saída Exemplo:
```
"12-03-2023"
```

## Mergulho Profundo
A biblioteca `clj-time` era a escolha padrão para manipulação de data e hora no Clojure até a adoção do `java.time`, introduzido no Java 8, que é agora considerado o padrão de fato devido a sua imutabilidade e API mais rica. Contudo, `clj-time` ainda é usada, especialmente em projetos mais antigos ou por aqueles que preferem uma interface mais Clojure-esque.

Alternativas incluem usar diretamente a API `java.time` através da interoperabilidade Java ou explorar outras bibliotecas da comunidade Clojure como `tick`, que tem uma abordagem moderna e funcional para datas e horas.

Quanto aos detalhes técnicos, manipular datas no passado e no futuro geralmente envolve criar datetimes imutáveis que são transformados por operações que adicionam ou subtraem períodos de tempo como dias, meses ou anos. A imutabilidade é crucial porque garante que as operações de data/hora não mudarão o estado original, o que é consistente com a filosofia do Clojure de evitar efeitos colaterais.

## Veja também
- Documentação oficial do Clojure sobre datas e horas: [link]
- Biblioteca clj-time no GitHub: https://github.com/clj-time/clj-time
- Guia de uso da biblioteca Tick: https://juxt.pro/tick/docs/index.html
- Artigo sobre interoperabilidade Java com Clojure: [link]
