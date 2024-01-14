---
title:    "Clojure: Obtendo a data atual."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por que utilizar a data atual em programas Clojure?

Muitos programas requerem a utilização da data atual, seja para registro de logs, agendamento de tarefas ou até mesmo para exibir ao usuário. Com o Clojure, é possível obter a data atual de forma simples e eficiente.

## Como Obter a Data Atual em Clojure

Utilizar a data atual em programas Clojure é fácil, basta seguir os seguintes passos:

1. Importe a biblioteca `java.util.Date`
2. Utilize a função `now` da biblioteca `java-time` para obter a data atual
3. Converta a data para o formato desejado utilizando a função `format` da biblioteca `java-time`

Veja um exemplo de código abaixo:

```
(ns meu-programa.data
  (:import [java.util Date]))
  
(require '[java-time :as time])

(defn get-data-atual []
  (let [data-atual (Date.) ;; obtém a data atual
        data-formatada (time/format data-atual "dd/MM/yyyy")] ;; converte a data para o formato desejado
    data-formatada)) ;; retorna a data formatada

(get-data-atual) ;; retorna a data atual no formato "dd/MM/yyyy"
```

Output:
```
"28/10/2021" ;; data atual no formato "dd/MM/yyyy"
```

## Mergulho Profundo: Entendendo o Código

Vamos explorar um pouco mais sobre como obter a data atual em Clojure. No código acima, utilizamos a função `now` da biblioteca `java-time` para obter a data atual. Essa função retorna um objeto `Instant`, que representa um momento específico no tempo.

Em seguida, utilizamos a função `format`, também da biblioteca `java-time`, para converter o objeto `Instant` em uma string no formato desejado. No exemplo acima, utilizamos o formato "dd/MM/yyyy", mas é possível utilizar outros formatos, como "yyyy-MM-dd" ou "MM/dd/yyyy".

É importante lembrar que a função `format` retorna uma string, por isso é necessário utilizar aspas na hora de definir a variável `data-formatada`.

# Veja Também

- [Documentação oficial do java-time](https://clojure.github.io/java-time/)
- [Tutorial sobre como utilizar datas em Clojure](https://clojure.org/guides/dates)
- [Outras bibliotecas úteis para manipulação de datas em Clojure](https://cljdoc.org/search?q=topic:date)

Agora você já sabe como obter a data atual em programas Clojure. Esperamos que este artigo tenha sido útil e que você possa utilizá-lo em seus projetos futuros. Até a próxima!