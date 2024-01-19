---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Converter uma data em uma string é transformar um objeto de data, que é uma representação numérica de uma data, em um simples texto. Programadores fazem isso para facilitar a apresentação e manipulação de datas.

## Como Fazer:

Veja um exemplo de como é feito o processo em Clojure:

```Clojure
(use 'clj-time.format)

(defn converte-data-para-string [data]
(let [formatter (clj-time.format/formatter "dd/MM/yyyy")]
(clj-time.format/unparse formatter data)))

(converte-data-para-string (clj-time.core/now))
```
Este código irá imprimir a data atual como uma string no formato "dd/MM/yyyy". Aqui, o método `now` é usado para retornar a data e o horário atual.

##  Mergulho Profundo

Historicamente, diferentes linguagens de programação têm diferentes métodos para converter datas em strings. Clojure, como uma linguagem moderna, provê maneiras eficientes e simples para esta conversão. 

Existem alternativas como usar a função Java nativa `SimpleDateFormat`, ainda que esta seja mais difícil de manipular e ofereça menos flexibilidade em comparação ao pacote `clj-time`.

A implementação detalhada envolve conversão de objetos de data em objects 'java.util.date' e então formatá-los em String através da especificação do formato desejado.

## Veja Também

Para maiores informações, cheque essas fontes:
- Documentação Oficial Clojure para lidar com datas: http://clojuredocs.org/clojure.core/date
- Documentação do pacote `clj-time`: https://github.com/clj-time/clj-time
- Para aprender mais sobre a função `SimpleDateFormat` do Java: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html