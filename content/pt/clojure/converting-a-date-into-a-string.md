---
title:                "Convertendo uma data em uma string."
html_title:           "Clojure: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que
 
Convertendo uma data em uma string em Clojure pode ser útil em várias situações, como por exemplo exibir a data em um formato específico para apresentação aos usuários ou armazenar a data formatada em um banco de dados.
 
## Como fazer
 
Para converter uma data em uma string, podemos utilizar a função `format` do namespace `clojure.string`, que aceita como parâmetros a data e um formato de string. Veja um exemplo abaixo:
 
```Clojure
(require '[clojure.string :as string])
 
(def data (java.util.Date.))
(string/format data "dd/MM/yyyy")
```
 
Este código retorna uma string no formato "dd/MM/yyyy", que representa dia, mês e ano da data atual. Para mais detalhes sobre os formatos de string disponíveis, consulte a documentação da função `format`.
 
## Deep Dive
 
A conversão de datas em strings pode ser realizada de diversas maneiras em Clojure, desde o uso da função `format` até a criação de funções personalizadas com a ajuda de outras bibliotecas, como a `clj-time`. Além disso, é importante ter em mente as diferenças entre datas e instâncias de `java.util.Date` e `java.time.LocalDateTime`, pois podem afetar o resultado da conversão. Consulte a documentação e explore as diferentes opções para encontrar a melhor abordagem para o seu caso.
 
## Veja também
 
- Documentação oficial sobre a função `format`: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/format
- Tutorial sobre a biblioteca `clj-time`: https://clj-time.github.io/clj-time/doc/index.html