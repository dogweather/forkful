---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Trabalhando Com a Data Atual em Clojure

## O Que & Por Que?
Identificar a data atual se trata de obter a data e a hora precisas do momento na execução do programa. Isso é crucial para os programadores, pois permite rastrear eventos, gerar registros de data e hora e realizar tarefas baseadas em tempo.

## Como Fazer:
Aqui está um exemplo de como obter a data e hora atual em Clojure:

```Clojure
(ns myapp.core)

(defn -main
  []
  (println (java.util.Date.)))
```
Ao executar, deve imprimir algo como:
```Clojure
Thu Jan 13 22:13:46 BRST 2022
```

## Mergulho Profundo
Na verdade, a manipulação de datas remonta aos primeiros dias da computação. No passado, tal tarefa era notoriamente difícil devido às várias idiossincrasias associadas a fusos horários e calendários da vida real.

Existem alternativas para obter a data atual em Clojure. Você pode usar a biblioteca `clj-time` que fornece uma interface rica para lidar com datas e horários.

Quando `(java.util.Date.)` é chamado, ele retorna um objeto `java.util.Date` que representa o instante atual até o milissegundo mais próximo. Clojure, sendo uma linguagem que roda na JVM, pode interagir com código e classes do Java.

## Veja Também:
- Documentação Java para `java.util.Date`: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
- Biblioteca `clj-time`: https://github.com/clj-time/clj-time
- Tutorial Clojure para manipulação de datas: https://www.tutorialspoint.com/clojure/clojure_date_time.htm