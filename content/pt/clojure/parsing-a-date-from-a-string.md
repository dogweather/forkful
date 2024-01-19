---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O quê e Por quê?
Em programação, analisar uma data de uma string significa converter essa string em um objeto de data. Isso é feito para manipulação mais fácil e eficiente de informações de data.

## Como fazer:
Em Clojure, usamos a biblioteca clj-time para essa tarefa. Aqui está um exemplo de código:

```Clojure
(ns meu-projeto.core
  (:require [clj-time.core :as t]
            [clj-time.format :as f]))

(defn string-para-data [s]
  (f/parse (f/formatters :date-time-no-ms) s))

(println (string-para-data "2016-03-05T15:30:00Z"))
```

Este código irá analisar a string de data e exibir o resultado. O objeto de data gerado será:

```
#object[org.joda.time.DateTime 0x6fef9fe5 "2016-03-05T15:30:00.000Z"]
```

## Mergulho Profundo
Clojure não fornece a funcionalidade de análise de data fora da caixa, e esse é o motivo pelo qual precisamos usar bibliotecas externas, como clj-time, que é uma camada fina sobre Joda-Time, uma biblioteca Java bem respeitada para manipulação de data e hora. Uma alternativa ao usar clj-time poderia ser a interoperação direta com a API Java Date e SimpleDateFormat, mas clj-time oferece uma interface mais agradável e idiomática.

Com relação aos detalhes de implementação, em clj-time, a string é dada a um objeto *parser* configurado com um formatters específico (`:date-time-no-ms` no nosso exemplo), que reconhece o formato da string de data. Esse *parser* converte a string em um objeto DateTime, que pode ser manipulado e formatado de várias maneiras.

## Veja também
1. Para a documentação completa da biblioteca clj-time, veja [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
2. Para uma introdução mais aprofundada à analise de data e hora em Clojure, veja [https://www.baeldung.com/clojure-date-time](https://www.baeldung.com/clojure-date-time)
3. Para uma compreensão mais profunda sobre a biblioteca Joda-Time que está por baixo do clj-time, veja [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)