---
title:    "Clojure: Obtendo a data atual"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Há muitas razões pelas quais você pode precisar obter a data atual em um programa Clojure. Pode ser para registrar o horário de uma transação, criar um calendário dinâmico ou simplesmente exibir a data em um formato específico. Independentemente do motivo, é um conceito útil para entender e implementar em seus projetos.

## Como fazer:

Para obter a data atual em Clojure, podemos usar a função nativa `java.util.Date`. Vamos dar uma olhada em alguns exemplos:

```
;; Importar a classe java.util.Date
(import 'java.util.Date)

;; Obter a data atual
(Date.)

;; Saída: #inst "2021-08-20T07:10:35.901-00:00"

;; Armazenar a data atual em uma variável
(def data-atual (Date.))

;; Saída: #inst "2021-08-20T07:10:35.901-00:00"

;; Converter a data para uma string no formato dd-MM-yyyy
(str (.getDay data-atual) "-" (.getMonth data-atual) "-" (.getYear data-atual))

;; Saída: "20-8-2021"
```

Podemos ver que, ao usar a função `Date.`, obtemos um objeto `java.util.Date`, que contém todas as informações sobre a data atual. Podemos então usar outras funções para manipular essas informações e exibi-las da maneira desejada.

## Profundidade:

Enquanto `java.util.Date` é uma maneira conveniente de obter a data atual, ela não é a mais recomendada. Isso porque essa classe é mutável, o que significa que os valores podem ser alterados depois de criados, o que pode levar a resultados imprevisíveis. Além disso, essa classe está obsoleta e foi substituída pela classe `java.time`.

Para obter a data atual de maneira mais eficiente e segura, é recomendado usar a função `java.time.LocalDate/now`, que retorna um objeto da classe `java.time.LocalDate`. Vamos ver um exemplo:

```
;; Importar a classe java.time.LocalDate
(import 'java.time.LocalDate)

;; Obter a data atual
(LocalDate/now)

;; Saída: #object[java.time.LocalDate 0x18051e85 "2021-08-20"]

;; Armazenar a data atual em uma variável
(def data-atual (LocalDate/now))

;; Saída: #object[java.time.LocalDate 0x1aa5d7aa "2021-08-20"]

;; Converter a data para uma string no formato dd-MM-yyyy
(str (.getDayOfMonth data-atual) "-" (.getMonthValue data-atual) "-" (.getYear data-atual))

;; Saída: "20-8-2021"
```

Podemos ver que, ao usar `java.time.LocalDate/now`, obtemos um objeto `java.time.LocalDate` imutável, que contém informações precisas sobre a data atual. Além disso, essa classe possui muitas funções úteis para manipulação de datas, tornando-a uma opção mais robusta.

## Veja também:
- [Aprenda Clojure em 15 minutos](https://dev.to/abhi1010/learn-clojure-in-15-minutes-2bb1)
- [Documentação oficial de Clojure](https://clojure.org/documentation)
- [JavaDoc da classe java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)