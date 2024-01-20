---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:56.366029-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Obter a data atual em Clojure significa acessar informações do sistema sobre o momento presente. Programadores fazem isso para registrar eventos, gerar timestamps e para funcionalidades que dependem do tempo real.

## Como fazer:
```Clojure
;; Importando a classe necessária
(import java.util.Calendar)

;; Função para pegar a data e hora atuais
(defn get-current-date-time []
  (let [calendar (Calendar/getInstance)]
    (println (str "Data e hora atual: "
                  (.get calendar Calendar/YEAR) "-"
                  (.get calendar Calendar/MONTH) "-"
                  (.get calendar Calendar/DATE) " "
                  (.get calendar Calendar/HOUR_OF_DAY) ":"
                  (.get calendar Calendar/MINUTE) ":"
                  (.get calendar Calendar/SECOND)))))

;; Executando a função
(get-current-date-time)
```
**Saída esperada (varia com o momento da execução):**
```
Data e hora atual: 2023-3-25 14:32:47
```

## Mergulho Profundo
Historicamente, manipular datas e horas em Java e Clojure podia ser complexo e propenso a erros. Isso mudou com a introdução do Joda-Time e, posteriormente, da API `java.time` no Java 8, oferecendo uma melhoria significativa. Clojure, sendo uma linguagem hospedada na JVM, também se beneficia dessas melhorias. Alternativas à classe `Calendar` incluem `java.time.LocalDateTime` e `java.time.ZonedDateTime`. A implementação costuma envolver chamadas interop a essas bibliotecas Java, mas é possível utilizar bibliotecas Clojure específicas como `clj-time`.

## Veja Também
- Documentação oficial do Clojure: [https://clojure.org/](https://clojure.org/)
- Biblioteca clj-time no GitHub: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Guia sobre a API java.time: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)