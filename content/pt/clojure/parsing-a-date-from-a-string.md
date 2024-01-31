---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:36:08.367417-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Analisar uma data a partir de uma string é o processo de interpretar texto que representa uma data para que possa ser usada como um tipo de dado de data/tempo em programas. Programadores fazem isso porque frequentemente lidam com datas em formatos de string e precisam manipulá-las para armazenamento, comparação ou cálculos.

## Como fazer:
Para analisar uma data em Clojure, você pode usar a biblioteca `clj-time`, que é uma fina camada sobre a `Joda-Time`. Primeiro, adicione a dependência ao seu `project.clj`:

```clojure
[clj-time "0.15.2"]
```

Depois, use a função `parse` da `clj-time.format` com um padrão de formatação para converter sua string em um objeto de data/tempo:

```clojure
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

(defn parse-date [date-string pattern]
  (let [formatter (f/formatters pattern)]
    (f/parse formatter date-string)))

(println (parse-date "2023-03-25" "yyyy-MM-dd"))
;; #object[org.joda.time.DateTime 0x... "2023-03-25T00:00:00.000Z"]
```

## Aprofundamento
Análise de data a partir de strings não é algo exclusivo de Clojure; é uma necessidade comum na maioria das linguagens de programação devido à vasta gama de fontes de dados que podem incluir datas como strings, como bancos de dados, APIs e arquivos de usuário. 

Historicamente, Clojure, assim como muitas outras linguagens na JVM, dependeu do Joda-Time, uma biblioteca robusta para manipulação de data e hora até a introdução do `java.time` (JSR-310) no Java 8, inspirado pelo Joda-Time. O `clj-time` ainda é muito utilizado, mas programadores também podem optar por usar `java.time` diretamente.

Alternativamente, ao invés de usar uma biblioteca externa, você pode interagir diretamente com `java.time` do Java:

```clojure
(import java.time.LocalDate)
(import java.time.format.DateTimeFormatter)

(defn parse-date-java [date-string]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (-> date-string
        (LocalDate/parse formatter))))

(println (parse-date-java "2023-03-25"))
;; 2023-03-25
```

Detalhes de implementação podem incluir o tratamento de exceções para strings malformadas e o suporte a fusos horários ou localidades específicas, que também são suportados pelo `clj-time` e `java.time`.

## Veja também
- Documentação oficial do `clj-time`: https://github.com/clj-time/clj-time
- Guia de usuário de `Joda-Time`: http://www.joda.org/joda-time/userguide.html
- Documentação do `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Postagem no blog sobre transição do Joda-Time para o `java.time`: https://blog.joda.org/2014/11/converting-from-joda-time-to-javatime.html
