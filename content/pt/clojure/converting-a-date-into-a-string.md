---
title:                "Clojure: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Por que

Converter uma data em uma string é uma habilidade importante para qualquer programador, pois permite a apresentação de informações de datas de maneira legível para os usuários. Além disso, ao trabalhar com APIs ou bancos de dados, pode ser necessário usar uma string para representar uma data.

##Como fazer

Para converter uma data em uma string, podemos usar a função `format` da biblioteca `java.time`. Primeiro, importamos a biblioteca usando `:import`, em seguida, utilizamos a função `format` para formatar a data da forma desejada.

```Clojure
(ns meu-programa
  (:import [java.time LocalDate])
  
(def data (LocalDate/now)) ;criando uma data atual
(def string-data (java.time.format.DateTimeFormatter/ISO_DATE
                   (java.time.format.DateTimeFormatter/format data))) ;convertendo a data em uma string no formato ISO
(prn string-data) ;imprimindo a string no console
```

A saída será algo como: `2021-05-23`.

##Mergulhando mais fundo

A função `format` pode receber como parâmetros a data, um padrão de formatação e um `LocalDate`. Se nenhum padrão for especificado, será utilizado o formato padrão da classe `DateTimeFormatter`. Além disso, você também pode usar `DateTimeFormatter` para criar padrões personalizados para representar diferentes formatos de data.

Outra opção é utilizar a função `str`, que transforma qualquer valor em uma string. No entanto, é importante notar que ela pode não fornecer o formato desejado e pode gerar erros ao tentar fazer operações de data posteriormente.

No geral, é importante entender bem o formato de data desejado e utilizar a função `format` corretamente para garantir a exatidão e consistência dos dados.

##Veja também

- Documentação da função `format` da biblioteca `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#format-java.time.temporal.TemporalAccessor-
- Guia completo de formatos de datas disponíveis: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns