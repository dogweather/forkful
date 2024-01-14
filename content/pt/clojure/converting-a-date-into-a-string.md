---
title:                "Clojure: Convertendo uma data em uma string"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Por que converter uma data em uma string?

Converter uma data em uma string pode ser útil em diversas situações, como em aplicações que exigem a apresentação de datas de forma legível e amigável para os usuários. Além disso, pode ser necessário converter uma data em uma string para realizar operações de formatação e manipulação de dados em sistema de banco de dados.

Como fazer:

```Clojure
(ns date-to-string.core
  (:require [clojure.string :as str]))

;; Exemplo de conversão de data em string
(str (java.util.Date.))
;; Output: "Mon Sep 06 15:04:20 GMT 2021"

```

Para converter uma data em uma string em Clojure, é necessário primeiro importar a biblioteca "clojure.string" com o comando ":require". Em seguida, pode-se utilizar a função "str" para realizar a conversão, passando a data em formato "java.util.Date" como argumento. O resultado será uma string contendo a data atual.

Além disso, é possível realizar conversões personalizadas utilizando funções como "format" e "simple-date-format", que permitem especificar o formato da data a ser apresentada. Isso é útil para atender às necessidades específicas de cada aplicação.

Deep Dive:

Para realizar uma conversão mais detalhada de uma data em uma string, é importante entender como o Clojure trata os tipos de dados de data e hora. Em Clojure, uma data é representada pelo tipo "java.util.Date", enquanto uma hora é representada pelo tipo "java.sql.Time". Ambos os tipos são simples objetos Java, o que facilita a conversão entre eles e para outros tipos de dados.

É importante ter atenção aos diferentes formatos de data e hora suportados pelo Java, que podem variar de acordo com o idioma e localização do sistema operacional. É possível especificar o formato da data utilizando a função "SimpleDateFormat", que oferece diversas opções de formatação, como dia, mês, ano, hora, minuto e segundo.

Outra opção interessante é a função "subseq", que permite extrair parte da string convertida, facilitando a formação de data formatada. Por exemplo, ```(subseq (str (java.util.Date.)) 11 19)``` resultará em "GMT 2021", que pode ser útil para apresentar apenas o ano da data, por exemplo.

See Also:

- [Documentação oficial do Clojure](https://clojure.org/)
- [Funções de data e hora do Clojure](https://clojuredocs.org/clojure.core/date-time)
- [Manipulação de strings em Clojure](https://clojuredocs.org/clojure.string)