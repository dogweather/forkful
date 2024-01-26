---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever no erro padrão (stderr) é direcionar saída de erros do programa para um canal específico, permitindo separá-la da saída normal (stdout). Programadores fazem isso para melhorar a depuração e o monitoramento, separando mensagens de erro de outros tipos de output.

## How to:
Em Clojure, você pode escrever para o stderr utilizando `binding` e o `*err*` stream. Veja:

```clojure
(binding [*out* *err*]
  (println "Este é um erro!"))
```

Saída esperada no stderr: `Este é um erro!`

Um exemplo mais prático seria usando função para log de erros:

```clojure
(defn log-erro [mensagem]
  (binding [*out* *err*]
    (println mensagem)))

(log-erro "Algo deu errado!")
```

Essa função direcionará a mensagem "Algo deu errado!" para o stderr.

## Deep Dive
Historicamente, a separação de stderr e stdout vem da era dos terminais Unix, onde era útil separar saídas de erros da saída regular para tratamentos distintos. Em Clojure, isso é feito por meio da variável `*err*`, que é um java.io.Writer especializado em stderr. Alternativas incluem o uso de bibliotecas de logging, como `clojure.tools.logging` ou `log4j`, que oferecem mais flexibilidade e funcionalidades do que simplesmente imprimir no stderr. A implementação direta no Clojure abstrai consideravelmente a complexidade comparada a usar funções de baixo nível em outras linguagens.

## See Also
- Documentação oficial de Clojure sobre `*out*` e `*err*`: [Clojure - difference between *out* and *err*](https://clojure.org/reference/vars#_out_err)
- Clojure para Java devs: [Clojure from the ground up: debugging](https://aphyr.com/posts/319-clojure-from-the-ground-up-debugging)
- Biblioteca de logging clojure.tools.logging: [clojure.tools.logging](https://github.com/clojure/tools.logging)
