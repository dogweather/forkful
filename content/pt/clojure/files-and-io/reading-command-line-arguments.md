---
date: 2024-01-20 17:55:35.370171-07:00
description: "Como Fazer: Antigamente, acessar argumentos de linha de comando era\
  \ bagun\xE7ado, mas com Clojure, \xE9 simples. Outras linguagens t\xEAm abordagens\
  \ semelhantes,\u2026"
lastmod: '2024-04-05T21:53:46.535779-06:00'
model: gpt-4-1106-preview
summary: "Antigamente, acessar argumentos de linha de comando era bagun\xE7ado, mas\
  \ com Clojure, \xE9 simples."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como Fazer:
```Clojure
; Guarda os argumentos em uma variável
(def args *command-line-args*)

; Imprime os argumentos
(doseq [arg args]
  (println arg))

; Exemplo de uso:
; Execute na linha de comando: clj seu_script.clj arg1 arg2 arg3
; Saída esperada:
; arg1
; arg2
; arg3
```

## Mergulho Profundo
Antigamente, acessar argumentos de linha de comando era bagunçado, mas com Clojure, é simples. Outras linguagens têm abordagens semelhantes, mas a simplicidade funcional de Clojure torna tudo mais suave. A variável `*command-line-args*` contém uma lista dos argumentos fornecidos pelo usuário, prontinha pra você mexer o quanto quiser.

Alternativas incluem o uso de bibliotecas para parsing mais complexo, como [tools.cli](https://github.com/clojure/tools.cli), que ajuda no processamento de flags e opções estruturadas.

Quanto aos detalhes de implementação, Clojure vai atrás desses argumentos dentro da JVM, que é onde os programas Clojure rodam. Se você precisa de algo mais robusto, talvez vai ter que lidar com Java diretamente pra acessar as capacidades plenas da JVM.

## Veja Também
* [Clojure Docs - *command-line-args*](https://clojuredocs.org/clojure.core/*command-line-args*)
* [Tutorial Clojure - Leitura de argumentos de linha de comando](https://www.braveclojure.com/getting-started/)
* [Library tools.cli para parsing de argumentos de linha de comando](https://github.com/clojure/tools.cli)
