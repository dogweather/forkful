---
title:                "Lendo uma data de uma string"
html_title:           "Clojure: Lendo uma data de uma string"
simple_title:         "Lendo uma data de uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Para programar em Clojure, é importante entender como lidar com datas. Uma tarefa comum é a análise de uma data que está em formato de string para transformá-la em um tipo de dado que possa ser manipulado pelo código.

Os programadores geralmente fazem isso ao processar informações de entrada, como formulários de usuários, arquivos de texto ou dados de bancos de dados, onde as datas são apresentadas em formatos diferentes.

## Como fazer:

Para analisar uma data de uma string em Clojure, usamos a função `parse` do namespace `java.time`. Aqui está um exemplo:

```Clojure
(require '[java.time :as time])

(time/parse "2020-06-16")
```
Este comando irá retornar uma instância de `Instant`, que representa um momento específico no tempo.

Se quisermos extrair informações específicas da data, podemos usar o método `atZone` para converter o `Instant` em um `ZonedDateTime`:

```Clojure
(time/atZone (time/parse "2020-06-16") (java.time.ZoneId/of "America/Sao_Paulo"))
```

Isso irá retornar um objeto `ZonedDateTime` contendo a informação da data para a zona de tempo especificada.

## Mergulho profundo:

A análise de datas em formato de string é uma tarefa comum em programação, que se tornou mais simples com o avanço das linguagens modernas. Anteriormente, os programadores precisavam criar suas próprias funções para lidar com esse tipo de análise, resultando em códigos mais complexos e propensos a erros.

Além disso, é importante notar que cada linguagem possui suas próprias funções para manipulação de datas, por isso é importante conhecer bem as ferramentas disponíveis em Clojure para realizar essa tarefa de forma eficiente.

Outra alternativa para análise de datas em Clojure é o uso da biblioteca `clj-time`, que oferece uma API mais amigável para manipulação de datas.

## Veja também:

- Documentação oficial de `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Biblioteca `clj-time`: https://github.com/clj-time/clj-time