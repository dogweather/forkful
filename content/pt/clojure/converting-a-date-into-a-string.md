---
title:                "Converter uma data em uma string"
html_title:           "Clojure: Converter uma data em uma string"
simple_title:         "Converter uma data em uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Converter uma data em uma string é o processo de transformar uma data (formato de dados) em uma sequência de caracteres (formato de texto). Isso é comumente feito para permitir que as datas sejam exibidas ou manipuladas em diferentes formatos, como DD/MM/YYYY ou MM/DD/YYYY. Os programadores frequentemente convertem datas em strings para facilitar a visualização e manipulação de dados em seus programas.

## Como fazer:
```Clojure
;; Importar a biblioteca java.time para trabalhar com datas
(ns minha-aplicação
  (:import (java.time LocalDateTime)))

;; Criar uma instância de LocalDateTime com a data atual
(def data-atual (LocalDateTime/now))

;; Converter a data em uma string no formato DD/MM/YYYY
(str (LocalDateTime/format data-atual (java.time.format.DateTimeFormatter/ofPattern "dd/MM/yyyy")))
```

Saída: "24/09/2021"

## Profundando:
Converter datas em strings tem sido uma tarefa importante para os programadores desde o início do desenvolvimento de software. Antigamente, essa conversão era feita manualmente, usando métodos matemáticos complexos para calcular o tempo decorrido desde um ponto inicial pré-determinado. No entanto, com o avanço da tecnologia, foram criados padrões e bibliotecas específicas para a manipulação de datas, facilitando muito o trabalho dos programadores.

Uma alternativa à conversão de datas em strings é o uso de objetos de data diretamente em programas, sem a necessidade de strings intermediárias. No entanto, dependendo dos objetivos do programa e do tipo de dados que estão sendo trabalhados, a conversão pode ser mais eficiente e prática.

A implementação da conversão de datas em strings em Clojure é feita usando a biblioteca padrão java.time, que fornece métodos e classes específicas para trabalhar com datas e horas. A partir da versão 1.8, Clojure passou a suportar toda a funcionalidade dessa biblioteca, o que torna a conversão de datas em strings muito mais fácil e intuitiva.

## Veja também:
- [Documentação da biblioteca java.time em Clojure] (https://clojure.org/reference/java_interop#date_time)
- [Tutorial de formatação de datas em Clojure] (https://practicalli.github.io/clojure/working-with-dates-and-times/formatting-dates-and-times.html)
- [Uma visão geral sobre a manipulação de datas em Clojure] (https://www.baeldung.com/clojure-dates)